defmodule Advent2024.Day3Test do
  use ExUnit.Case

  alias Advent2024.Day3
  doctest Day3

  test "solve day 3a" do
    input =
      """
      xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
      """

    assert 161 == Day3.solve_a(input: input)
  end

  test "solve day 3b" do
    input =
      """
      xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
      """

    assert 48 == Day3.solve_b(input: input)
  end
end
