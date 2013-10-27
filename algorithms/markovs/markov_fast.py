#
# corpus är en dict med strängnycklar och listelement, texempel liek dis:
#     corpus["jag har"] = ["aldrig", "en", "försökt", "en", "en", "försökt"]
#
# så när man vill ha ett nästa ord kan man i princip bara
#     random.choice(corpus[previous_two])
#
# för att corpus ska äta mindre minne kan man byta design lite till tyyyyp:
#     corpus["jag har"]["aldrig"] = 1
#     corpus["jag har"]["en"] = 3
#     corpus["jag har"]["försökt"] = 2
#
# alternativt lagra tuples av (förekomst, ord) i listan istället för att ha dict-in-dict!
# dict i dict är probably mycket lättare att implementera, men jag vet inte hur mycket
# minnesoverhead dicts har i sig.
#

def markov_fast(corpus):
    """performs randomised lookups in corpus until it's got a sentence"""
    sentence = random.choice(list(corpus.keys())).split()
    while len(sentence) < 16:
        nextword = random.choice(corpus[' '.join(sentence[-2:])])
        if not nextword:
            break
        sentence.append(nextword)
    return ' '.join(sentence)

if __name__ == '__main__':
    times = []
    corpus = {}

    with open('#adun.se.log', 'r') as f:
        cf = f.readlines()
        # extract the necessary information from each line
        for line in cf:
            if '> ' in line:
                words = line.split('> ', 1)[1].split()
                # for each couple of words, add them to the dict with a following word
                if len(words) > 2:
                   for i in range(2, len(words)):
                       key = ' '.join(words[i-2:i])
                       if key not in corpus:
                           corpus[key] = []
                       corpus[key].append(words[i])
                key = ' '.join(words[-2:])
                if key not in corpus:
                    corpus[key] = []
                # the end of the line is indicated by an empty word
                corpus[key].append('')

    #########
    # the rest is the same as before
    # just for measuring times

    for i in range(0, 25):
        start = time.time()
        print(markov_fast(corpus))
        end = time.time()
        times.append(end - start)

    times.sort()
    quarter = int(len(times)/4)

    min = times[0]
    q1 = times[1*quarter]
    q2 = times[2*quarter]
    q3 = times[3*quarter]
    max = times[-1]


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
