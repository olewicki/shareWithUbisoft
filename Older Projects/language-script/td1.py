## TP1
# Author   : Doriane Olewicki -- 1970976
# Due date : 18 Jan 2019
import pyphen
import sys
import re

filename = re.sub(r'\.txt', r'', sys.argv[1]) # get the filename without ".txt"
file = open(filename + "_solution.txt", "w") # creates the output-file

dic = pyphen.Pyphen(lang='en_us')

with open(sys.argv[1], "r") as f:
    for i, line in enumerate(f):
        phrases = re.split(r'(?<=[\?\.\!]) ', line) # splits on spaces after special characters
        words = [re.sub(r'(s)',r" \1", p) for p in phrases] # put a space in from of special characters to ease the words' detection
        print(words)
        words = [e
                for p in words
                for e in re.split(r' ', p) # split at spaces
                if (e != "")] # remove empty strings
        syl = [e
                for w in words
                for e in re.split(r"-", dic.inserted(w))] # split each word at "-" after using pyphen

        # file printing :
        if (i > 0) :
            file.write("\n\n")
        line_nbr = "Line " + str(i+1)
        file.write(line_nbr + "\n" + "-" * len(line_nbr) +"\n")
        file.write("Syllables:\n" + "-".join(syl) + "Total: " + str(len(syl)) + "\n\n")
        file.write("Words:\n" + "-".join(words) + "Total: " + str(len(words)) + "\n\n")
        file.write("Sentences:\n" + "-".join(phrases) + "Total: " + str(len(phrases)) + "\n\n")

        tot = 206.835 - 1.015 * (len(words) / len(phrases)) - 84.6 * (len(syl) / len(words)) # Flesh computation
        file.write("Total: %.3f\n" % tot)
file.close()
