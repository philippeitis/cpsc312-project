import json
from http.server import BaseHTTPRequestHandler,HTTPServer, SimpleHTTPRequestHandler
import sys
from urllib.parse import urlparse, parse_qs

import spacy


# Global data (NLP MODEL, plus cache for efficiency)
MODEL = spacy.load("en_core_web_sm")
CACHE = {}


class Sentence:
    __slots__ = ["doc", "tokens"]

    def __init__(self, sentence: str):
        """Contains tokenized data for sentence."""
        self.doc = MODEL(sentence)
        self.tokens = [
            {"text": token.text, "tag": str(token.tag_)}
            for token in MODEL(sentence)
        ]

    def similarity(self, other: "Sentence"):
        """Returns the similarity between self and other"""
        return self.doc.similarity(other.doc)

def process_sentence(sentence: str):
    """Processes the sentence, returning a cached result if possible"""
    if sentence in CACHE:
        processed = CACHE[sentence]
    else:
        processed = Sentence(sentence)
        CACHE[sentence] = processed
    return processed

class NlpHandler(SimpleHTTPRequestHandler):
    """A very simple HTTP server for serving tokenized/POS tagged sentences"""
    def do_GET(self):
        parsed_path = urlparse(self.path)
        try:
            # NLP endpoint does tokenization / POS tagging
            if parsed_path.path == "/nlp":
                # Get sentence from query string
                sentence = parse_qs(parsed_path.query)["sentence"][0]
                sentence = process_sentence(sentence)
                self.send_response(200)
                self.send_header('Content-type', 'application/json')
                self.end_headers()
                self.wfile.write(bytes(json.dumps({"sentence": sentence.tokens}), "utf8"))
            # Similarity endpoint: Computes similarity from 0 to 1 using cosine distance
            # between aggregated word vectors of either sentence
            elif parsed_path.path == "/similarity":
                # Read two sentences to compare
                sent_a, sent_b = parse_qs(parsed_path.query)["sentence"]
                sent_a = process_sentence(sent_a)
                sent_b = process_sentence(sent_b)
                self.send_response(200)
                self.send_header('Content-type', 'application/json')
                self.end_headers()
                self.wfile.write(bytes(json.dumps({"similarity": sent_a.similarity(sent_b)}), "utf8"))
            else:
                # Unexpected path - user error, 404
                self.send_response(404)
                self.end_headers()
                self.wfile.write(b"Unexpected path")
        except KeyError:
            # User error again
            self.send_response(400)
            self.end_headers()
            self.wfile.write(b"User did not provide sentence keys")
        # Other exceptions are handled by parent class


if __name__ == "__main__":
    with HTTPServer(("localhost", 8080), NlpHandler) as httpd:
        httpd.serve_forever()


