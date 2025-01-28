defmodule Advent2024 do
  @moduledoc """
  Documentation for `Advent2024`.
  """

  @days [
    Advent2024.Day1
  ]

  @doc """
  Run the solution for a day
  """
  def run(day) do
    day_mod = Enum.at(@days, day - 1)
    apply(day_mod, :solve_a, [])
  end
end
