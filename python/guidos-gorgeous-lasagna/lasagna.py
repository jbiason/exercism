EXPECTED_BAKE_TIME = 40
PREPARATION_TIME = 2


def bake_time_remaining(elapsed: int) -> int:
    """Calculate the bake time remaining.

    :param elapsed_bake_time: int baking time already elapsed.
    :return: int remaining bake time derived from 'EXPECTED_BAKE_TIME'.

    Function that takes the actual minutes the lasagna has been in the oven as
    an argument and returns how many minutes the lasagna still needs to bake
    based on the `EXPECTED_BAKE_TIME`.
    """
    return EXPECTED_BAKE_TIME - elapsed


def preparation_time_in_minutes(layers: int) -> int:
    """Calculate the preparation time for the number of layers.

    :param layers: Number of layers.
    :return: Preparation time in minutes.
    """
    return PREPARATION_TIME * layers


def elapsed_time_in_minutes(layers: int, elapsed: int) -> int:
    """Calculate total elapsed cooking time.

    :param layers: Number of layers.
    :param elapsed: Elapsed time in minutes.

    :return: Total elapsed time.
    """
    return preparation_time_in_minutes(layers) + elapsed
