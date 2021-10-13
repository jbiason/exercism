""" Meltdown Mitigation exercise """

from typing import Union
from typing import Literal

EFFICIENCY_GREEN = 'green'
EFFICIENCY_ORANGE = 'orange'
EFFICIENCY_RED = 'red'
EFFICIENCY_BLACK = 'black'
EFFICIENCY = Literal[EFFICIENCY_GREEN, EFFICIENCY_ORANGE, EFFICIENCY_RED,
                     EFFICIENCY_BLACK]

FAIL_SAFE_LOW = 'LOW'
FAIL_SAFE_NORMAL = 'NORMAL'
FAIL_SAFE_DANGER = 'DANGER'
FAIL_SAFE = Literal[FAIL_SAFE_LOW, FAIL_SAFE_NORMAL, FAIL_SAFE_DANGER]


def is_criticality_balanced(temperature: Union[int, float],
                            neutrons_emitted: Union[int, float]) -> bool:
    """Verify criticality is balanced.

    :param temperature: temperature value (integer or float)
    :param neutrons_emitted: number of neutrons emitted per second (integer or float)
    :return:  boolean True if conditions met, False if not

    A reactor is said to be critical if it satisfies the following conditions:
    - The temperature is less than 800.
    - The number of neutrons emitted per second is greater than 500.
    - The product of temperature and neutrons emitted per second is less than 500000.
    """
    return (temperature < 800
            and neutrons_emitted > 500
            and ((neutrons_emitted * temperature) < 500000))


def reactor_efficiency(voltage: Union[int, float],
                       current: Union[int, float],
                       theoretical_max_power: Union[int, float]) -> EFFICIENCY:
    """Assess reactor efficiency zone.

    :param voltage: voltage value (integer or float)
    :param current: current value (integer or float)
    :param theoretical_max_power: power that corresponds to a 100% efficiency (integer or float)
    :return: str one of 'green', 'orange', 'red', or 'black'

    Efficiency can be grouped into 4 bands:

    1. green -> efficiency of 80% or more,
    2. orange -> efficiency of less than 80% but at least 60%,
    3. red -> efficiency below 60%, but still 30% or more,
    4. black ->  less than 30% efficient.

    The percentage value is calculated as
    (generated power/ theoretical max power)*100
    where generated power = voltage * current
    """
    generated_power = voltage * current
    efficiency = (generated_power / theoretical_max_power) * 100

    efficiency_color = EFFICIENCY_BLACK
    if efficiency >= 80:
        efficiency_color = EFFICIENCY_GREEN
    elif efficiency >= 60:
        efficiency_color = EFFICIENCY_ORANGE
    elif efficiency >= 30:
        efficiency_color = EFFICIENCY_RED

    return efficiency_color


def fail_safe(temperature: Union[int, float],
              neutrons_produced_per_second: Union[int, float],
              threshold: Union[int, float]):
    """Assess and return status code for the reactor.

    :param temperature: value of the temperature (integer or float)
    :param neutrons_produced_per_second: neutron flux (integer or float)
    :param threshold: threshold (integer or float)
    :return: str one of: 'LOW', 'NORMAL', 'DANGER'

    - `temperature * neutrons per second` < 90% of `threshold` == 'LOW'
    - `temperature * neutrons per second` +/- 10% of `threshold` == 'NORMAL'
    - `temperature * neutrons per second` is not in the above-stated ranges ==  'DANGER'
    """
    heated_neutrons = temperature * neutrons_produced_per_second
    efficiency = (heated_neutrons / threshold) * 100

    state = FAIL_SAFE_DANGER
    if efficiency < 90:
        state = FAIL_SAFE_LOW
    elif efficiency >= 90 and efficiency <= 110:
        state = FAIL_SAFE_NORMAL

    return state
