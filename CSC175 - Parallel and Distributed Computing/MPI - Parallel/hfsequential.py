# Import time module
import time

# record start time
start = time.time()

contents = open('textfile.txt', 'r')
string = contents.read()

counter = {}
for letter in string:
    if letter not in counter:
        counter[letter] = 0
    counter[letter] += 1

print(counter)

end = time.time()

print("The time of execution of above program is :",
      (end-start) * 10**3, "ms")

print("--- %s seconds ---" % (time.time() - end))


