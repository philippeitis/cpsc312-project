import sys
from functools import cached_property

import spacy

# Global data (NLP MODEL, plus cache for efficiency)
MODEL = spacy.load("en_core_web_md")
CACHE = {}
LEV_CACHE = {}

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


def min_ind(a, b, c):
    if a <= b:
        if a <= c:
            return (a, 0)
        else:
            return (c, 2)
    else:
        if b <= c:
            return (b, 0)
        else:
            return (c, 2)


def backtrack(table, connections):
    ind_a = len(table) - 1
    distance = min(table[-1][1:])
    ind_b = table[-1][1:].index(distance) + 1
    path = [(ind_a - 1, ind_b - 1)]

    while True:
        prev = connections[ind_a][ind_b]
        if prev is None:
            return path[:-1][::-1], distance
        elif prev == 0:
            ind_a -= 1
        elif prev == 1:
            ind_b -= 1
        elif prev == 2:
            ind_a -= 1
            ind_b -= 1
        path.append((ind_a - 1, ind_b - 1))


def fuzzy_substring_match(sent_a, sent_b):
    if (sent_a, sent_b) in LEV_CACHE:
        return LEV_CACHE[(sent_a, sent_b)]
    
    if len(sent_a) > len(sent_b):
        is_reversed=True
        sent_a, sent_b = sent_b, sent_a
    else:
        is_reversed=False
    table = [[0] * (len(sent_b) + 1) for _ in range(len(sent_a) + 1)]
    connections = [[None] * (len(sent_b) + 1) for _ in range(len(sent_a) + 1)]
    for (j, cb) in enumerate(sent_b):
        j += 1
        for (i, ca) in enumerate(sent_a):
            i += 1
            if ca == cb:
                subst_cost = 0
            elif ca == cb.lower():
                subst_cost = 0.3
            else:
                subst_cost = 1            
            table[i][j], connections[i][j] = min_ind(
                table[i - 1][j] + 1,
                table[i][j - 1] + 1,
                table[i - 1][j - 1] + subst_cost
            )
    
    path, distance = backtrack(table, connections)
    if is_reversed:
        path = [(y, x) for x, y in path]
    LEV_CACHE[(sent_a, sent_b)] = (path, distance)
    return path, distance


if __name__ == "__main__":
    # REPL
    while True:
        try:
            line = sys.stdin.readline().strip()
        except (BrokenPipeError, IOError):
            sys.exit()
        components = line.split(" ")
        # similarity len_0 len_1
        if components[0] == "similarity":
            sent_a = read_sentence(int(components[1]))
            sent_b = read_sentence(int(components[2]))
            # Find most similar sentence in doc
            similarity = max(sent.similarity(sent_b) for sent in sent_a.sentences())
            sys.stdout.write("OK\n")
            sys.stdout.write(f"{similarity}\n")
        elif components[0] == "lev_similarity":
            sent_a = sys.stdin.read(int(components[1]))
            sent_b = sys.stdin.read(int(components[2]))
            # Find most similar part of doc
            _, distance = fuzzy_substring_match(sent_a, sent_b)
            similarity = 1.0 - (distance / min(len(sent_a), len(sent_b)))
            sys.stdout.write("OK\n")
            sys.stdout.write(f"{similarity}\n")
        else:
            sys.stdout.write("ERR\n")
        # Very important, since this is not done automatically
        sys.stdout.flush()
