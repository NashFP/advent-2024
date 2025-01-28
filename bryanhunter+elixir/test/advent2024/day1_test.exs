defmodule Advent2024.Day1Test do
  use ExUnit.Case

  alias Advent2024.Day1
  doctest Day1

  test "solve day 1a" do
    input =
      """
      3   4
      4   3
      2   5
      1   3
      3   9
      3   3
      """

    assert 11 == Day1.solve_a(input: input)
  end

  test "solve day 1b" do
    input =
      """
      3   4
      4   3
      2   5
      1   3
      3   9
      3   3
      """

    assert 31 == Day1.solve_b(input: input)
  end
end
