import sys
from functools import cached_property

import spacy

# Global data (NLP MODEL, plus cache for efficiency)
MODEL = spacy.load("en_core_web_md")
CACHE = {}


class Sentence:
    __slots__ = ["doc", "tokens", "_sentences"]

    def __init__(self, sentence: str):
        """Contains tokenized data for sentence."""
        self.doc = MODEL(sentence)
        self.tokens = [
            {"text": token.text, "tag": str(token.tag_)}
            for token in MODEL(sentence)
        ]
        self._sentences = None

    def similarity(self, other: "Sentence"):
        """Returns the similarity between self and other"""
        return self.doc.similarity(other.doc)

    def sentences(self):
        """Returns all of the sentences within self. Uses caching to avoid repeating work"""
        if self._sentences is None:
            self._sentences = [process_sentence(sentence.text.strip()) for sentence in self.doc.sents]
        return self._sentences


def process_sentence(sentence: str):
    """Processes the sentence, returning a cached result if possible"""
    if sentence in CACHE:
        processed = CACHE[sentence]
    else:
        processed = Sentence(sentence)
        CACHE[sentence] = processed
    return processed


def read_sentence(size: int):
    return process_sentence(sys.stdin.read(size))


if __name__ == "__main__":
    # REPL
    while True:
        line = sys.stdin.readline().strip()
        components = line.split(" ")
        # similarity len_0 len_1
        if components[0] == "similarity":
            sent_a = read_sentence(int(components[1]))
            sent_b = read_sentence(int(components[2]))
            # Find most similar sentence in doc
            similarity = max(sent.similarity(sent_b) for sent in sent_a.sentences())
            sys.stdout.write("OK\n")
            sys.stdout.write(f"{similarity}\n")
        else:
            sys.stdout.write("User error")
        # Very important, since this is not done automatically
        sys.stdout.flush()
