Long before [ITA Software](http://www.itasoftware.com) being acquired by [Google](http://www.google.com/), they were known with a set of mind blowing recruiting puzzles they use in advertising campaigns around Boston and Cambridge. Below is an example campaign poster one you could have seen on the walls of Boston subway stations.

![Solve this, work here.](https://raw.github.com/vy/ita-puzzles/master/solve-this-work-here.gif)

Due to [company's high profile of its Lisp usage](http://www.franz.com/success/customer_apps/data_mining/itastory.lhtml), puzzles were something like a giant Lisp-programmer magnet, which eventually attracted me as well. After Google acquisition, puzzles are dismissed in the recruiting process and their presence started to disappear in the web. In this repository, I keep a record of a couple of puzzles with my own solutions.

# BitVector Genealogy 

The BitVectors are an ancient and immortal race of 10,000, each with a 10,000 bit genome. The race evolved from a single individual by the following process: 9,999 times a BitVector chosen at random from amongst the population was cloned using an error-prone process that considers each bit independently, and flips it with 20% probability.

Write a program to guess the reproductive history of BitVectors from their genetic material. The randomly-ordered file [bitvectors-genes.data.gz](https://raw.github.com/vy/ita-puzzles/master/bitvectors-genes.data.gz) contains a 10,000 bit line for each individual. Your program's output should be, for each input line, the 0-based line number of that individual's parent, or -1 if it is the progenitor. Balance performance against probability of mistakes as you see fit.

To help you test your program, here is a much smaller 500x500 input dataset: [bitvectors-genes.data.small.gz](https://raw.github.com/vy/ita-puzzles/master/bitvectors-genes.data.small.gz), along with its solution file: [bitvectors-parents.data.small](https://raw.github.com/vy/ita-puzzles/master/bitvectors-parents.data.small).

Solution: [bitvector-genealogy.lisp](https://raw.github.com/vy/ita-puzzles/master/bitvector-genealogy.lisp)

# Sling Blade Runner

*"How long a chain of overlapping movie titles, like Sling Blade Runner, can you find?"*

Use the following listing of movie titles: [movies.txt](https://raw.github.com/vy/ita-puzzles/master/movies.txt). Multi-word overlaps, as in "License to Kill a Mockingbird," are allowed. The same title may not be used more than once in a solution. Heuristic solutions that may not always produce the greatest number of titles will be accepted: seek a reasonable tradeoff of efficiency and optimality.

Data provided by [MovieLens](http://www.movielens.umn.edu/) at the University of Minnesota.

*This puzzle was created March 2007 and retired September 2008.*

Solution: [sbr-footprint.lisp](https://raw.github.com/vy/ita-puzzles/master/sbr-footprint.lisp), [sbr-random.lisp](https://raw.github.com/vy/ita-puzzles/master/sbr-random.lisp)

# Word Numbers

*"If the integers from 1 to 999,999,999 are written as words, sorted alphabetically, and concatenated, what is the 51 billionth letter?"*

To be precise: if the integers from 1 to 999,999,999 are expressed in words (omitting spaces, 'and', and punctuation[1]), and sorted alphabetically so that the first six integers are

    eight
    eighteen
    eighteenmillion
    eighteenmillioneight
    eighteenmillioneighteen
    eighteenmillioneighteenthousand

    twothousandtwohundredtwo

then reading top to bottom, left to right, the 28th letter completes the spelling of the integer "eighteenmillion".

The 51 billionth letter also completes the spelling of an integer. Which one, and what is the sum of all the integers to that point?

[1] For example, 911,610,034 is written "ninehundredelevenmillionsixhundredtenthousandthirtyfour"; 500,000,000 is written "fivehundredmillion"; 1,709 is written "onethousandsevenhundrednine".

*This puzzle was created March 2007 and retired September 2008.*

Solution: [word-numbers.lisp](https://raw.github.com/vy/ita-puzzles/master/word-numbers.lisp)
