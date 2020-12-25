import random

rand = random.Random(0)  # set seed                                                                                                                            
shit = "L."
size = 750
with open("11-big.dat", "w") as f:
    for _ in range(size):
        line = "".join(rand.choice(shit) for _ in range(size))
        f.write(line)
        f.write("\n")
