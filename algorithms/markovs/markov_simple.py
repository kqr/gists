
def nextwords(corpus, words):
    """
    Return all the possible next words when given a few words to start with.
    (Searches the entire file for say, "foo bar" and when it finds
    "foo bar baz" it returns "baz" and then continues to search.)
    """
    for line in corpus:
        if words + ' ' in line:
            yield line.split(words, 1)[1].split()[0]



with open('file_with_lines.txt', 'r') as f:
    corpus = f.readlines()

# Choose a random line to start with, and split it into a list of words
seed = random.choice(corpus).split(' ')

# Use the two first words to start sentence generation
sentence = seed[:2]

while len(sentence) < 16:
    # Candidate words are all the words that can follow the last two
    # words in the sentence. (According to the text provided)
    candidates = list(nextwords(corpus, ' '.join(sentence[-2:])))
    nextword = random.choice(candidates)
    sentence.append(nextword)

# Return all the words, space separated
return ' '.join(sentence)
