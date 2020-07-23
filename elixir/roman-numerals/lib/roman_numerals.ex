defmodule RomanNumerals do
  @letters [
    %{:unitary => "I", :halfway => "V", :next => "X"},
    %{:unitary => "X", :halfway => "L", :next => "C"},
    %{:unitary => "C", :halfway => "D", :next => "M"},
    %{:unitary => "M", :halfway => "" , :next => ""}
  ]

  @doc """
  Convert the number to a roman number.
  """
  @spec numeral(pos_integer) :: String.t()
  def numeral(number) do
    numeral(number, @letters)
  end

  defp numeral(0, _letters) do
    ""
  end

  defp numeral(number, [current | next]) do
    next_iteration = Integer.floor_div(number, 10)
    lower_part = number - (next_iteration * 10)
    numeral(next_iteration, next) <> letter(lower_part, current)
  end

  defp letter(single_value, map) do
    case single_value do
      0 -> ""
      1 -> String.duplicate(map[:unitary], 1)
      2 -> String.duplicate(map[:unitary], 2)
      3 -> String.duplicate(map[:unitary], 3)
      4 -> map[:unitary] <> map[:halfway]
      5 -> map[:halfway]
      6 -> map[:halfway] <> String.duplicate(map[:unitary], 1)
      7 -> map[:halfway] <> String.duplicate(map[:unitary], 2)
      8 -> map[:halfway] <> String.duplicate(map[:unitary], 3)
      9 -> map[:unitary] <> map[:next]

      # OR...
      # x when x == 0 -> ""
      # x when x >= 1 and x <= 3 -> String.duplicate(map[:unitary], x)
      # x when x == 4 -> map[:unitary] <> map[:halfway]
      # x when x == 5 -> map[:halfway]
      # x when x > 5 and x <= 8 -> map[:halfway] <> String.duplicate(map[:unitary], x - 5)
      # _ -> map[:unitary] <> map[:next]
    end
  end
end
