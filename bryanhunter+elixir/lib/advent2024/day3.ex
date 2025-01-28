defmodule Advent2024.Day3 do
  def solve_a(opts) do
    input = Keyword.get(opts, :input, get_input_a())

    input
    |> String.split("mul(", trim: true)
    |> Enum.map(fn x ->
      case Integer.parse(x) do
        {num_a, <<",", rest::binary>>} ->
          case Integer.parse(rest) do
            {num_b, <<")", _rest::binary>>} ->
              num_a * num_b

            _ ->
              []
          end

        _ ->
          []
      end
    end)
    |> List.flatten()
    |> Enum.sum()
  end

  def solve_b(opts) do
    input = Keyword.get(opts, :input, get_input_a())

    new_input =
      ("don't()do()" <> input)
      |> String.split("don't()")
      |> Enum.map(fn x ->
        String.split(x, "do()", parts: 2)
        |> Enum.drop(1)
        |> Enum.join()
      end)
      |> Enum.join()

    solve_a(input: new_input)
  end

  defp get_input_a() do
    File.read!("../data/day3a.txt")
  end
end
