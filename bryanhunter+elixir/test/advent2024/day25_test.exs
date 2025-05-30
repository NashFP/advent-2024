defmodule Advent2024.Day25Test do
  use ExUnit.Case

  alias Advent2024.Day25
  doctest Day25

  test "solve day 25a" do
    input =
      """
      #####
      .####
      .####
      .####
      .#.#.
      .#...
      .....

      #####
      ##.##
      .#.##
      ...##
      ...#.
      ...#.
      .....

      .....
      #....
      #....
      #...#
      #.#.#
      #.###
      #####

      .....
      .....
      #.#..
      ###..
      ###.#
      ###.#
      #####

      .....
      .....
      .....
      #....
      #.#..
      #.#.#
      #####
      """

    assert 3 == Day25.solve_a(input: input)
  end
end
