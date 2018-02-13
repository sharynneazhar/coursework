import string
import time
import itertools

LETTERS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
ETAOIN = 'ETAOINSHRDLCUMWFGYPBVKJXQZ'

def vigenere(message, key):
  """Runs the vigenere cipher and returns decrypted text"""
  decrypted_text = ''
  
  # create tuple of message and key
  mk_tuples = zip(message, itertools.cycle(key))

  # generate vigenere result
  for mk_tuple in mk_tuples:
    loc = reduce(lambda x, y: LETTERS.index(x) - LETTERS.index(y), mk_tuple)
    decrypted_text += LETTERS[loc % 26]
  
  return decrypted_text


def find_key(key_len, word, dict):
  """Finds the decryption key based on brute force dictionary attack"""
  possible_keys = [''.join(l) for l in itertools.product(LETTERS, repeat = key_len)]
  print 'Possible Keys: %i' % len(possible_keys)
  for possible_key in possible_keys:
    if vigenere(word, possible_key) in dict:
      return possible_key


def decrypt(message, key_len, first_word_len, dictionary):
  """Decrypts an encrypted message and returns the decrypted text"""  
  print '\nMessage: %s' % message
  print 'Key Length: %i' % key_len
  print 'First Word Length: %i' % first_word_len

  # get the first words possibilities based on first word length
  first_words_list = list(filter(lambda word: len(word) == first_word_len, dictionary))
  print 'Possible First Words: %i' % len(first_words_list)

  # get encrypted first word
  encrypted_first_word = message[0:first_word_len]

  # find the decryption key
  key = find_key(key_len, encrypted_first_word, first_words_list)

  # decrypt the text
  text = vigenere(message, key)

  print '>> Key: %s' % key
  print '>> Decrypted Text: %s' % text


def run_tests(dictionary):
  with open('test_cases.txt') as test_file:
    test_cases = test_file.readlines()
  
  test_cases = map(lambda line: line.strip('\n'), test_cases)

  for test in test_cases:
    test = test.split()
    start = time.time()
    decrypt(test[0], int(test[1]), int(test[2]), dictionary)
    end = time.time()
    print '>> Time Taken: %.4f seconds' % (end - start)


if __name__ == '__main__':
  """Main program"""
  dictionary = []

  # read the dictionary contents into a list
  with open('dict.txt') as dict_file:
    dictionary = dict_file.readlines()

  # strip whitespaces if any
  dictionary = list(map(lambda line: line.strip(), dictionary))

  # get user input
  print 'Menu'
  print '1) Run Tests'
  print '2) Decode Your Own'

  choice = int(input('Your choice: '))
  if choice == 1:
    run_tests(dictionary)
  else:
    message = raw_input('Enter your message: ').upper()
    key_len = int(input('Enter the decryption key length: '))
    first_word_len = int(input('Enter the first word length: '))

    # run the decryption
    start = time.time()
    decrypt(message, key_len, first_word_len, dictionary)
    end = time.time()
    print '>> Time Taken: %f' % (end - start)
