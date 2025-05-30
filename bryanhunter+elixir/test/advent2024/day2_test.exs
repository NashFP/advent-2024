defmodule Advent2024.Day2Test do
  use ExUnit.Case

  alias Advent2024.Day2
  doctest Day2

  test "solve day 2a" do
    input =
      """
      7 6 4 2 1
      1 2 7 8 9
      9 7 6 2 1
      1 3 2 4 5
      8 6 4 4 1
      1 3 6 7 9
      """

    assert 2 == Day2.solve_a(input: input)
  end

  # test "solve day 2b"
end
