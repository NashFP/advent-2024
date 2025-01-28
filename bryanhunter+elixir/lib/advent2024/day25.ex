defmodule Advent2024.Day25 do
  def solve_a(opts) do
    input = Keyword.get(opts, :input, get_input_a())
    locks_and_keys = get_locks_and_keys(input)

    combos =
      for key <- locks_and_keys.keys,
          lock <- locks_and_keys.locks,
          do: {lock, key}

    combos
    |> Stream.filter(fn {lock, key} -> key_opens_lock?(key, lock) end)
    |> Enum.count()
  end

  def key_opens_lock?(key, lock) do
    max =
      Nx.add(key, lock)
      |> Nx.reduce_max()
      |> Nx.to_number()

    max < 2
  end

  def get_locks_and_keys(input) do
    input
    |> String.replace("#", <<1>>)
    |> String.replace(".", <<0>>)
    |> String.split(~r{\s\s+}, trim: true)
    |> Stream.map(fn x ->
      t1 =
        x
        |> String.split("\n", trim: true)
        |> Enum.map(&String.to_charlist/1)
        |> Nx.tensor(names: [:y, :x], type: :u8)

      type =
        if t1[0] |> Nx.all() |> Nx.to_number() == 1 do
          :locks
        else
          :keys
        end

      {height, width} = Nx.shape(t1)
      tensor = Nx.slice(t1, [1, 0], [height - 2, width])
      {type, tensor}
    end)
    |> Enum.group_by(fn {k, _} -> k end, fn {_, t} -> t end)
    |> Map.new()
  end

  def solve_b(_opts) do
  end

  defp get_input_a() do
    File.read!("../data/day25a.txt")
  end
end
