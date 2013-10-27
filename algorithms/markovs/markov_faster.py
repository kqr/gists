import random, time


def markov_posting(corpus, postings, pickiness):
    # Pick pickiness words to start with as initial state
    sentence = random.choice(corpus).split()[:pickiness]

    # Limit the length of a sentence.
    while len(sentence) < 16:
        # For each iteration, choose as state only the last pickiness words
        words = sentence[-pickiness:]

        # Reduce the searched lines to only the ones containing all pickiness words.
        relevant = set(postings[words[0]])
        for word in words[1:]:
            relevant &= set(postings[word])

        # List the possible next words.
        nextwords = []
        for index in relevant:
            try: nextwords.append(corpus[index].split(' '.join(words) + ' ', 1)[1].split()[0])
            except: continue

        if nextwords: nextword = random.choice(nextwords)
        else: break

        if nextword: sentence.append(nextword)
        else: break

    return ' '.join(sentence)

    

if __name__ == '__main__':
    # corpus is a list of all the lines of the file (or conversation, if updated live)
    corpus = []
    # postings is a dictionary from words to line numbers (String -> [Int])
    postings = {}

    # Measuring time to build dictionary
    start = time.time()


    # Read the file, but keep only the relevant data,
    # in other words the lines people have written.
    with open('#adun.se.log', 'r') as f:
        corpus = [line.split('> ', 1)[1] for line in f.readlines() if '> ' in line] 

    # For every line, split it into words and populate the postings
    # dictionary with the correct line numbers for each word.
    for (i, line) in enumerate(corpus):
        words = line.split()
        for word in words:
            if word not in postings:
                postings[word] = []
            postings[word].append(i)


    # Stop measuring time to build dictionary
    end = time.time()
    preptime = end - start


    #########
    # Follows does the same as always.
    # It's just measuring times.

    times = []
    for i in range(0, 25):
        start = time.time()
        print(markov_posting(corpus, postings, 2))
        end = time.time()
        times.append(end - start)

    times.sort()
    quarter = int(len(times)/4)

    min = times[0]
    q1 = times[1*quarter]
    q2 = times[2*quarter]
    q3 = times[3*quarter]
    max = times[-1]


    print("Building dictionary took {:.2f} seconds.".format(preptime))
    print('Generated {} sentences in {:.2f} seconds. (average time was {:.2f} seconds)'.format(
        len(times),
        sum(times),
        sum(times)/len(times)))

    scaling = 50/(max - min)
    len1 = int((q1 - min)*scaling)
    len2 = int((q2 - q1)*scaling)
    len3 = int((q3 - q2)*scaling)
    len4 = int((max - q3)*scaling)

    print('<{:.2f}{}[{:.2f}{}|{:.2f}|{}{:.2f}]{}{:.2f}>'.format(min, '-'*len1, q1, '-'*len2, q2, '-'*len3, q3, '-'*len4, max))
