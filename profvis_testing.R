# profvis testing

# Global - data retrieving

library(profvis)
  profvis({ runApp('./') }
          , prof_output = './result')

  profvis(prof_input = './result')
