package scalation.animation

import CommandType.*

/** Lifecycle and bounded-retention checks, runnable without Swing. */
@main def liveAnimationStateTest (): Unit =
    def command (action: CommandType, id: Int, x: Double = 0.0) =
        AnimateCommand (action, id, null, s"$id", true, null, Array (x, 0.0), x)
    val state = new LiveAnimationState
    state.submit (command (CreateNode, 99))
    state.submit (command (CreateToken, 1))
    for i <- 1 to 100000 do state.submit (command (MoveToken, 1, i.toDouble))
    assert (state.size == 2)                           // one token plus one graph command
    val (_, graph, tokens) = state.snapshot ()
    assert (graph.size == 1 && tokens.size == 1)
    assert (tokens.head._2.pts(0) == 100000.0)
    state.submit (command (MoveToken, 1, 100000.0))
    assert (state.snapshot ()._3.head._2 eq tokens.head._2)  // stationary duplicate discarded
    assert (state.size == 1)
    state.submit (command (DestroyToken, 1))
    state.submit (command (MoveToken, 1, 100001.0))     // late move must not resurrect an exited car
    assert (state.snapshot ()._3.isEmpty)
    for id <- 2 to 10000 do
        state.submit (command (CreateToken, id))
        state.submit (command (DestroyToken, id))
    assert (state.size == 0)                           // no lifecycle history while display is paused

    val producer = new Thread (() => {
        for id <- 10001 to 20000 do
            state.submit (command (CreateToken, id))
            state.submit (command (MoveToken, id, 1.0))
            state.submit (command (DestroyToken, id))
    })
    producer.start ()
    while producer.isAlive do
        val (_, _, snapshot) = state.snapshot ()
        assert (snapshot.size <= 1)
        snapshot.foreach { (create, move) =>
            assert (move == null || move.eid == create.eid)
        }
    producer.join ()
    assert (state.size == 0)
    println ("LiveAnimationState: lifecycle, retention, stationary and concurrent snapshot checks passed")
end liveAnimationStateTest
