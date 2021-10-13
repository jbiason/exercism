def add_prefix_un(word: str) -> str:
    """This function takes `word` as a parameter and returns a new word with an
    'un' prefix.

    :param word: str of a root word
    :return:  str of root word with un prefix
    """
    return f'un{word}'


def make_word_groups(vocab_words):
    """This function takes a `vocab_words` list and returns a string with the
    prefix  and the words with prefix applied, separated by ' :: '.

    :param vocab_words: list of vocabulary words with a prefix.
    :return: str of prefix followed by vocabulary words with
             prefix applied, separated by ' :: '.
    """
    prefix = vocab_words[0]
    return ' :: '.join(
        [prefix] +
        [f'{prefix}{word}'
         for word
         in vocab_words[1:]]
    )


def remove_suffix_ness(word):
    """This function takes in a word and returns the base word with `ness`
    removed.

    :param word: str of word to remove suffix from.
    :return: str of word with suffix removed & spelling adjusted.
    """
    no_suffix = word[:-4]
    if no_suffix[-1] == 'i':
        no_suffix = no_suffix[:-1] + 'y'
    return no_suffix


def noun_to_verb(sentence, index):
    """A function takes a `sentence` using the vocabulary word, and the `index`
    of the word once that sentence is split apart.  The function should return
    the extracted adjective as a verb.

    :param sentence: str that uses the word in sentence
    :param index:  index of the word to remove and transform
    :return:  str word that changes the extracted adjective to a verb.
    """
    word = sentence.split()[index].replace('.', '')
    return f'{word}en'
