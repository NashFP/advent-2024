defmodule Advent2024.Day4Test do
  use ExUnit.Case

  alias Advent2024.Day4
  doctest Day4

  test "solve day 4a" do
    input =
      """
      MMMSXXMASM
      MSAMXMSMSA
      AMXSXMAAMM
      MSAMASMSMX
      XMASAMXAMM
      XXAMMXXAMA
      SMSMSASXSS
      SAXAMASAAA
      MAMMMXMMMM
      MXMXAXMASX
      """

    assert 18 == Day4.solve_a(input: input)
  end

  test "solve day 4b" do
    input =
      """
      MMMSXXMASM
      MSAMXMSMSA
      AMXSXMAAMM
      MSAMASMSMX
      XMASAMXAMM
      XXAMMXXAMA
      SMSMSASXSS
      SAXAMASAAA
      MAMMMXMMMM
      MXMXAXMASX
      """

    assert 9 == Day4.solve_b(input: input)
  end
end
