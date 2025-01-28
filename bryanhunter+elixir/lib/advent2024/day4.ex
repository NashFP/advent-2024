defmodule Advent2024.Day4 do
  @doc ~S"""
    How many times does XMAS appear left-to-right, right-to-left, top-down, bottom-up,
    or diagonally?
      iex> Day4.solve_a(input: ~s(XMAS\nMMXA\nAXAM\nSAMS\n))
      3

  """
  def solve_a(opts) do
    input = Keyword.get(opts, :input, get_input_a())
    data = input |> String.split("\n", trim: true)

    rows =
      data |> Enum.map(fn x -> x |> String.to_charlist() end)

    tensor = Nx.tensor(rows, names: [:y, :x], type: :u8)
    {cols, rows} = tensor.shape

    for x <- 0..(cols - 4), y <- 0..(rows - 4) do
      four_by_four =
        [
          [[y, x], [y, x + 1], [y, x + 2], [y, x + 3]],
          [[y, x], [y + 1, x], [y + 2, x], [y + 3, x]],
          [[y, x], [y + 1, x + 1], [y + 2, x + 2], [y + 3, x + 3]],
          [[y, x + 3], [y + 1, x + 2], [y + 2, x + 1], [y + 3, x]]
        ]

      bottom_edge = [
        [[y + 1, x], [y + 1, x + 1], [y + 1, x + 2], [y + 1, x + 3]],
        [[y + 2, x], [y + 2, x + 1], [y + 2, x + 2], [y + 2, x + 3]],
        [[y + 3, x], [y + 3, x + 1], [y + 3, x + 2], [y + 3, x + 3]]
      ]

      right_edge = [
        [[y, x + 1], [y + 1, x + 1], [y + 2, x + 1], [y + 3, x + 1]],
        [[y, x + 2], [y + 1, x + 2], [y + 2, x + 2], [y + 3, x + 2]],
        [[y, x + 3], [y + 1, x + 3], [y + 2, x + 3], [y + 3, x + 3]]
      ]

      cond do
        x == cols - 4 and y == rows - 4 ->
          Nx.gather(tensor, Nx.tensor(four_by_four ++ bottom_edge ++ right_edge))

        x == cols - 4 ->
          Nx.gather(tensor, Nx.tensor(four_by_four ++ right_edge))

        y == rows - 4 ->
          Nx.gather(tensor, Nx.tensor(four_by_four ++ bottom_edge))

        true ->
          Nx.gather(tensor, Nx.tensor(four_by_four))
      end
    end
    |> Stream.map(fn cell ->
      cell
      |> Nx.to_list()
      |> Enum.count(fn x -> x == ~c"XMAS" or x == ~c"SAMX" end)
    end)
    |> Enum.sum()
  end

  def solve_b(opts) do
    input = Keyword.get(opts, :input, get_input_a())
    data = input |> String.split("\n", trim: true)

    rows =
      data |> Enum.map(fn x -> x |> String.to_charlist() end)

    tensor = Nx.tensor(rows, names: [:y, :x], type: :u8)
    {cols, rows} = tensor.shape

    for x <- 0..(cols - 3), y <- 0..(rows - 3) do
      three_by_three =
        [
          [[y, x], [y + 1, x + 1], [y + 2, x + 2]],
          [[y, x + 2], [y + 1, x + 1], [y + 2, x]]
        ]

      Nx.gather(tensor, Nx.tensor(three_by_three))
    end
    |> Stream.map(fn cell ->
      cell
      |> Nx.to_list()
      |> case do
        [~c"MAS", ~c"MAS"] -> 1
        [~c"MAS", ~c"SAM"] -> 1
        [~c"SAM", ~c"MAS"] -> 1
        [~c"SAM", ~c"SAM"] -> 1
        _ -> 0
      end
    end)
    |> Enum.sum()
  end

  defp get_input_a() do
    File.read!("../data/day4a.txt")
  end
end
