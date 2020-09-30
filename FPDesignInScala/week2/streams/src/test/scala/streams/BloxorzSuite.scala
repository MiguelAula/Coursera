package streams

import org.junit._
import org.junit.Assert.assertEquals

import Bloxorz._

class BloxorzSuite {
  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal,"The solution must always lead to legal blocks")
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait Level0 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """o-o------
        |oSooo----
        |-Too-----
        |---------""".stripMargin

    val optsolution = List(Right, Down, Right)
  }

  trait Level2 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """----------
        |--S-------
        |--oooo----
        |--oooo----
        |--Tooo----""".stripMargin

    val optsolution = List(Down, Down)
  }


  @Test def `terrain function level 1 (10pts)`: Unit =
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }


  @Test def `find char level 1 (10pts)`: Unit =
    new Level1 {
      assertEquals(Pos(1, 1), startPos)
    }


  @Test def `optimal solution for level 1 (5pts)`: Unit =
    new Level1 {
      assertEquals(Block(goal, goal), solve(solution))
    }


  @Test def `optimal solution length for level 1 (5pts)`: Unit =
    new Level1 {
      assertEquals(optsolution.length, solution.length)
    }

  /* Neighbor tests (added a posteriori): */

  @Test def `neighbors with history`: Unit =
    new Level1 {
      val n1 = neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up)).toSet
      val n2 = Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      )
      assertEquals(n1,n2)
    }

  @Test def `new neighbors only`: Unit =
    new Level1 {
      val n1 = newNeighborsOnly(
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).to(LazyList),
        Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1))))
      val n2 = Set(
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).to(LazyList)
      assertEquals(n1,n2)
    }

  /* My tests: */
  @Test def `Block movement check`: Unit =
    new Level1 {
      assertEquals(Block(Pos(1,1),Pos(1,1)).right, Block(Pos(1,2),Pos(1,3)))
      assertEquals(Block(Pos(1,1),Pos(1,1)).down, Block(Pos(2,1),Pos(3,1)))
      assertEquals(Block(Pos(1,1),Pos(1,1)).down.right, Block(Pos(2,2),Pos(3,2)))
      assertEquals(Block(Pos(1,1),Pos(1,1)).down.right.down, Block(Pos(4,2),Pos(4,2)))
    }

  @Test def `level0 - pathsFromStart`: Unit =
    new Level0 {
      assertEquals(List(
        (Block(Pos(1,1),Pos(1,1)),List()),
        (Block(Pos(1,2),Pos(1,3)),List(Right)),
        (Block(Pos(1,4),Pos(1,4)),List(Right,Right)),
        (Block(Pos(2,2),Pos(2,3)),List(Down,Right)),
        (Block(Pos(2,1),Pos(2,1)),List(Left,Down,Right))
      ),pathsFromStart.toList)
    }

  @Test def `level0 - pathsToGoal`: Unit =
    new Level0 {
      assertEquals(
        List((Block(Pos(2,1),Pos(2,1)),List(Left,Down,Right))),
        pathsToGoal.toList)
    }

  @Test def `level2 - opt solution`: Unit =
    new Level2 {
      println(pathsToGoal.toList)
      assertEquals(optsolution,solution)
      //assertEquals(optsolution.length, solution.length)
    }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
