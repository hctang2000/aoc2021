import pandas as pd
lines = pd.read_csv("input.txt", header = None)

def advent_day1(n):
    lines_roll = lines.rolling(n).sum()
    lines_roll_shift = lines_roll.shift(-1)
    return (lines_roll_shift > lines_roll).sum()
    
advent_day1(1) #1154
advent_day1(3) #1127

