import sys
from functools import cached_property

# NLP Library, plus numpy for numerical computations
# Please note that numpy is installed with spaCy
import spacy
import numpy as np


def take_n(generator, n):
    for _ in range(n):
        yield next(generator)


def cosine_similarity(v1, v2):
    return np.dot(v1, v2) / (np.linalg.norm(v1) * np.linalg.norm(v2))


def clamp(num: float, bottom: float, top: float):
    return max(bottom, min(num, top))


class Sentence:
    # Global data (NLP model, plus cache for efficiency)
    CACHE = {}
    MODEL = None

    __slots__ = ["doc", "tokens", "_sentences"]

    def __init__(self, sentence: str):
        """Contains tokenized data for sentence."""
        self.doc = self.process(sentence)
        self.tokens = [
            {"text": token.text, "tag": str(token.tag_)}
            for token in self.doc
        ]
        self._sentences = None

    @classmethod
    def process(cls, sent: str):
        """Processes the sentence, loading the model if it has not yet
        been loaded."""
        if cls.MODEL is None:
            cls.MODEL = spacy.load("en_core_web_md")
        return cls.MODEL(sent)

    @classmethod
    def cached(cls, sent: str) -> "Sentence":
        """Returns an instance of Sentence which contains the processed data,
        using a cached result if one exists."""
        if sent in cls.CACHE:
            return cls.CACHE[sent]
        processed = Sentence(sent)
        cls.CACHE[sent] = processed
        return processed

    def similarity(self, other: "Sentence"):
        """Returns the similarity between self and other"""
        return self.doc.similarity(other.doc)

    def sentences(self):
        """Returns all of the sentences within self. Uses caching to avoid repeating work"""
        if self._sentences is None:
            self._sentences = [self.cached(sentence.text.strip()) for sentence in self.doc.sents]
        return self._sentences

    def vector_chunks(self, size: int):
        """Yields the word vectors for a sliding window of tokens with the given size."""
        if size > len(self.doc):
            yield self.doc.vector
            return

        head_iter = iter(self.doc)
        body_iter = iter(self.doc)
        curr_chunk = sum(token.vector for token in take_n(body_iter, size))
        yield curr_chunk
        for head, tail in zip(head_iter, body_iter):
            # numerical inaccuracy, lets gooooo
            curr_chunk -= head.vector
            curr_chunk += tail.vector
            yield curr_chunk

    def sub_similarity(self, sent: "Sentence"):
        """Yields the similarity between the sentence,
        and a sliding window of tokens with the given size in self."""
        for chunk in self.vector_chunks(len(sent.doc)): 
            yield clamp(cosine_similarity(chunk, sent.doc.vector), 0.0, 1.0)


def read_sentence(size: int) -> Sentence:
    return Sentence.cached(sys.stdin.read(size))


if __name__ == "__main__":
    # REPL
    while True:
        try:
            line = sys.stdin.readline().strip()
        except (BrokenPipeError, IOError):
            sys.exit(0)
        components = line.split(" ")
        # similarity len_0 len_1
        if components[0] == "similarity":
            sent_a = read_sentence(int(components[1]))
            sent_b = read_sentence(int(components[2]))
            # Find most similar sentence in doc
            similarity = max(sent.similarity(sent_b) for sent in sent_a.sentences())
            sys.stdout.write("OK\n")
            sys.stdout.write(f"{similarity}\n")
        elif components[0] == "sub_similarity":
            sent_a = read_sentence(int(components[1]))
            sent_b = read_sentence(int(components[2]))
            # Find most similar sequence in doc
            similarity = max(sent_a.sub_similarity(sent_b))
            sys.stdout.write("OK\n")
            sys.stdout.write(f"{similarity}\n")
        elif components[0] == "exit":
            sys.stdout.write("BYE\n")
            sys.stdout.flush()
            exit(0)
        else:
            sys.stdout.write("ERR\n")
        # Very important, since this is not done automatically
        sys.stdout.flush()
