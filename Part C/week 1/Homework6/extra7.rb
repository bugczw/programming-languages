## Solution template for Guess The Word practice problem (section 7)

require_relative './section-7-provided'

class ExtendedGuessTheWordGame < GuessTheWordGame
  ## YOUR CODE HERE
end

class ExtendedSecretWord < SecretWord

  def initialize word
    self.word = word
    self.pattern = (word.chars.map {|ch| if letter? ch then '-' else ch end}).join
  end

  def letter? ch
    ch.match(/^[[:alpha:]]$/)
  end

  def guess_letter! letter
    found = self.word.downcase.index letter.downcase
    if found
      start = 0
      while ix = self.word.downcase.index(letter.downcase, start)
        self.pattern[ix] = self.word[ix]
        start = ix + 1
      end
    end
    found
  end

  def valid_guess? guess
    guess.length == 1 and letter? guess and !self.pattern.downcase.index guess.downcase
  end
end

## Change to `false` to run the original game
if false
  ExtendedGuessTheWordGame.new(ExtendedSecretWord).play
else
  GuessTheWordGame.new(SecretWord).play
end
