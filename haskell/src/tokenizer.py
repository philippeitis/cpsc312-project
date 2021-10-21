import sys
import spacy

if __name__ == "__main__":
    nlp = spacy.load("en_core_web_sm")

    for token in nlp(sys.argv[1]):
        print(token.text)
        print(token.tag_)