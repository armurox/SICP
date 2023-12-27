def main():
    print(square_root(4, 1))

def square_root(x, g):
    if abs(g - ((g + x / g)) / 2) < 0.001:
        return g
    else:
        return square_root(x, (g + x / g) / 2)

if __name__ == "__main__":
   main()
