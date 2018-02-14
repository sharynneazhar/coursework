from __future__ import print_function
import string
import time
import itertools

LETTERS = string.ascii_uppercase

def vigenere(message, key):
  """Runs the vigenere cipher and returns decrypted text"""
  decrypted_text = ''

  # create tuple of message and key
  message_key_tuple = zip(message, itertools.cycle(key))

  # generate vigenere result
  for message_key_tuple in message_key_tuple:
    loc = reduce(lambda x, y: LETTERS.index(x) - LETTERS.index(y), message_key_tuple)
    decrypted_text += LETTERS[loc % 26]
  
  return decrypted_text


def find_key(key_len, word, dict):
  """Finds the decryption key based on brute force dictionary attack"""
  # generate a list of all possible keys based on key length
  possible_keys = [''.join(i) for i in itertools.product(LETTERS, repeat=key_len)]
  num_possible_keys = len(possible_keys)
  print('Possible Keys: %i' % num_possible_keys) 

  # iterate over possible keys and crack the code (INEFFICIENT!)
  for possible_key in possible_keys:
    if vigenere(word, possible_key) in dict:
      return possible_key


def decrypt(message, key_len, first_word_len, dictionary):
  """Decrypts an encrypted message and returns the decrypted text"""  
  print('\nMessage: %s' % message)
  print('Key Length: %i' % key_len)
  print('First Word Length: %i' % first_word_len)

  # get the first words possibilities based on first word length
  first_words_list = filter(lambda word: len(word) == first_word_len, dictionary)
  print('Possible First Words: %i' % len(first_words_list))

  # get encrypted first word
  first_word = message[0:first_word_len]

  # find the decryption key
  start = time.time()
  key = find_key(key_len, first_word, first_words_list)
  end = time.time()
  find_key_time = end - start

  # decrypt the text
  start = time.time()
  plaintext = vigenere(message, key)
  end = time.time()
  vignere_time = end - start

  print('>> Key: %s' % key)
  print('>> Decrypted Text: %s' % plaintext)
  print('>> Time Taken to Find Key: %.4f seconds' % find_key_time)
  print('>> Time Taken to Decipher: %.4f seconds' % vignere_time)
  print('>> Total Time Taken: %.4f seconds\n' % (find_key_time + vignere_time))


if __name__ == '__main__':
  """Main program"""
  dictionary = []

  # read the dictionary contents into a list
  with open('dict.txt') as d_file:
    dictionary = d_file.readlines()
  dictionary = map(lambda d: d.strip(), dictionary)
  
  # read test cases provided into a list
  with open('test_cases.txt') as test_file:
    test_cases = test_file.readlines()
  test_cases = map(lambda tc: tc.strip('\n'), test_cases)

  for test in test_cases:
    test = test.split()
    decrypt(test[0], int(test[1]), int(test[2]), dictionary)

