defmodule WordCount do
  @doc """
  Count the number of words in the sentence.

  Words are compared case-insensitively.
  """

  @spec count(String.t()) :: map
  def count(sentence) do
    sentence
    |> String.split(~r/[ _ ]/)
    |> Enum.reduce(%{}, fn (word, acc) ->
      lower_word = word
      |> String.downcase
      |> String.replace(~r/[\$!&^%,:@]/, "")

      if String.length(lower_word) > 0 do
        count = Map.get(acc, lower_word, 0)
        Map.put(acc, lower_word, count + 1)
      else
        acc
      end
    end)
  end
end
