defmodule Day4 do

  @xmas %{0 => 88, 1 => 77, 2 => 65, 3 => 83}
  @input File.read!("../data/day4a.txt")

  def part1 do
    grid = get_grid()

    grid
    |> Map.keys()
    |> Enum.reduce(0, fn {x, y}, count ->
      if grid[{x, y}] == 88 do
        matches =
          for xd <- -1..1, yd <- -1..1 do
            !(xd == 0 and yd == 0) and match(grid, x + xd, y + yd, xd, yd, 1)
          end
          |> Enum.filter(& &1)
          |> Enum.count()

        count + matches
      else
        count
      end
    end)
  end

  def part2 do
    grid = get_grid()

    grid
    |> Map.keys()
    |> Enum.reduce(0, fn {x, y}, count ->
      with 65 <- grid[{x, y}],
           ul = grid[{x - 1, y - 1}],
           lr = grid[{x + 1, y + 1}],
           ur = grid[{x + 1, y - 1}],
           ll = grid[{x - 1, y + 1}],
           true <- [ul, lr] in [[77, 83], [83, 77]],
           true <- [ur, ll] in [[77, 83], [83, 77]] do
        count + 1
      else
        _ -> count
      end
    end)
  end

  defp get_grid() do
    @input
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.flat_map(fn {line, y} ->
      String.to_charlist(line) |> Enum.with_index() |> Enum.map(fn {c, x} -> {{x, y}, c} end)
    end)
    |> Map.new()
  end

  defp match(_, _, _, _, _, 4), do: true

  defp match(grid, x, y, xd, yd, depth) do
    @xmas[depth] == grid[{x, y}] && match(grid, x + xd, y + yd, xd, yd, depth + 1)
  end

end