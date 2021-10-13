def eat_ghost(power_pellet_active: bool, touching_ghost: bool) -> bool:
    """Check if the user can eat a ghost.

    :param power_pellet_active: does the player have an active power pellet?
    :param touching_ghost: is the player touching a ghost?
    :return: True if the user can eat the ghost.
    """
    return power_pellet_active and touching_ghost


def score(touching_power_pellet: bool, touching_dot: bool) -> bool:
    """Check if the user scored a point.

    :param touching_power_pellet: does the player have an active power pellet?
    :param touching_dot: is the player touching a dot?
    :return: bool
    """
    return touching_power_pellet or touching_dot


def lose(power_pellet_active: bool, touching_ghost: bool):
    """Check if the user died for touching a ghost without the power pellet.

    :param power_pellet_active: does the player have an active power pellet?
    :param touching_ghost: is the player touching a ghost?
    :return: bool
    """
    return touching_ghost and not power_pellet_active


def win(has_eaten_all_dots: bool, power_pellet_active: bool,
        touching_ghost: bool) -> bool:
    """Check if the user won the game.

    :param has_eaten_all_dots: bool - has the player "eaten" all the dots?
    :param power_pellet_active: bool - does the player have an active power pellet?
    :param touching_ghost:  bool - is the player touching a ghost?
    :return: bool
    """
    return has_eaten_all_dots and not lose(power_pellet_active, touching_ghost)
