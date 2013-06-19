hackerschool
============

I'll store the code I write at Hacker School (https://www.hackerschool.com/) during summer 2013 in this repo.

log of awesome HS things:

6/4/2013: I finished a chunk of python code today! It was the first of the Matasano Crypto Challenges (http://www.matasano.com/articles/crypto-challenges/), and it was pretty fun.  However, per the rules of these challenges, I can't post my code here.  On to challenge #2!

6/5/2013:  4 crypto challenges down!  Got a code review (it was totally awesome).  Went over some real analysis in the afternoon, for a change of pace.  All good times..

6/10/2013:  5 crypto challenges completed!  In the challenge 6 vs. Alyssa matchup, however, Challenge 6 is ahead.  Not for long, though.  I hear the comeback train.

6/13/2013:  I have cracked the code in crypto challenge 6!  Sadly I used a bit of an "exhaustive search" method, but the more elegant method is failing me at the moment.  In the eantime, I've been programming the game of 500 in R.  It's pretty fun.  I have a non-GUI version working at the moment, but will be refactoring it into something beautiful in the coming days.  I'd also love to write it up in python, and I think it would be awesome to extend it to be interactive, be online, have AI, etc etc etc.......the possibilities are endless.

6/19/2013:  I've successfully implemented a command-line version of 500 in both R and python.  If you have R, you can play 500 by installing [devtools](https://github.com/hadley/devtools) from CRAN, then running these four lines of code:
```
library(devtools)
install_github("hackerschool", "alyssafrazee", subdir="R500")
library(R500)
play500()
```
If you don't know how to play 500, the rules are [here](http://en.wikipedia.org/wiki/500_(card_game)). The script "500.py" in this repo is the python version - just run it, and you'll be dropped into a game.