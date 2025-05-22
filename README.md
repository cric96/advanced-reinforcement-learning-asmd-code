# Introduction to Multi-Agent Reinforcement learning (MARL)
## Code for ASMD seminar
This repository shows some basic examples of multi-agent reinforcement learning. It contains:
a) facades for the multi-agent environment and environment dynamics definition
b) interfaces for agents definition
c) implementation of Q learning and Deep Q learning
The MARL topic is really broad and consists of several algorithms and environment dynamics. 
For the interested ones, I suggest reading the papers listed in this repository: [https://github.com/LantaoYu/MARL-Papers](https://github.com/LantaoYu/MARL-Papers). 

Also, consider to read the following book: https://www.marl-book.com/
If you have any questions, please feel free to contact me at gianluca[dot]aguzzi[at]unibo[dot]it

## Installation
To install the project, you need to have sbt installed on your machine and python >= 3.8
Firs of all, after cloning the repository, you need to create a virtual environment and install the requirements:
```shell
python3 -m venv env
source env/bin/activate
```
Now, you can install the requirements:
```shell
pip install -r requirements.txt
```

To run the project, you need to compile the scala code and then run the python code:
```shell
sbt compile
sbt run
```

If everything is ok, typying 13 you should see the GYM example shown in class.
## Structure
```mermaid
classDiagram

class Distribution~E~ {
    <<trait>>
    def sample(): E
}
class StochasticGame~State,Action~ {
    <<trait>>
    + def agents: Int
    + def initialState: Distribution~State~
    + def transitionFunction: Function2~State, Seq~Action~, Action~
    + def rewardFunction: Function3~State, Seq~Action~, State~, Seq~Double~~
}

class MultiAgentEnvironment~State,Action~ {
    <<trait>>
    def act(actions: Seq~Action~): State
    def state(): State
    def reset(): Unit
}


class Agent~State, Action~ {
    <<trait>>
    def act(state: State): Action
    def record(state: State, action: Action, reward: Double, nextState: State): Unit
    def reset(): Unit
}

class Scheduler {
    <<trait>>
    def episode: Int
    def step: Int
}

class SchedulerListener {
    <<trait>>
    def onEpisodeChange(episode: Int): Unit
    def onStepChange(episode: Int): Unit
}

class Learner~State, Action~ {
    <<trait>>
    + def mode: Training | Test
    + def trainingMode(): Unit
    + def testMode(): Unit
    # def improve(state: State, action: Action, reward: Double, nextState: State): Unit
}

class Simulation~State, Action~ {
    def simulate(episode: Int, episodeLength: Int, agents: Seq~Agent~State,Action~~): Unit
}

class Render~State~ {
    <<trait>>
    def render(/state: State): Unit
}

class DecayReference~V~ {
    # def update(): Unit
    def value: V
}


StochasticGame -- Distribution: << uses >>
StochasticGame --> MultiAgentEnvironment: << creates >>
Agent -- MultiAgentEnvironment: lives
Agent <|-- Learner
Learner <|-- QLearner
Learner <|-- DeepQLearner
Simulation o-- MultiAgentEnvironment
Simulation o-- Render
Simulation o-- Scheduler
Scheduler o-- SchedulerListener: listen
SchedulerListener -- DecayReference: << uses >>
QLearner -- DecayReference: << uses >>
DeepQLearner -- DecayReference: << uses >>
```
A `StochasticGame` extends an MDP (Markov Decision Process) to multi-agent systems, offering a framework for understanding the dynamics of a `MultiAgentEnvironment`. More details on Stochastic Games can be found [here](https://en.wikipedia.org/wiki/Stochastic_game). 

A `MultiAgentEnvironment` involves multiple `Agents` interacting within an environment and perceiving its `State`. If an `Agent` can improve itself through experience, it qualifies as a `Learner`.

To simulate a `MultiAgentEnvironment`, a `Simulation` is utilized. It conducts multiple runs, each defined by an `episodeLength`, using the specified agents.

### Examples
In this repository, I demonstrate two examples using the general structure outlined above:

- **Competitive Task, Rock Paper Scissors**: Two agents engage in the game of rock paper scissors. More details on this scenario are available [here](https://direct.mit.edu/isal/proceedings/alife2018/30/404/99610). I explore outcomes when both agents employ the same learning algorithm and scenarios where one agent has only partial observation of the environment. While a deeper discussion on equilibria and multi-agent dynamics would be beneficial, these topics are beyond the scope of this lesson. For further reading, consider the PhD thesis: [Many-agent Reinforcement Learning](https://discovery.ucl.ac.uk/id/eprint/10124273/).

- **Cooperative Task, Agent Alignment**: Here, `N` agents aim to align themselves either in a row or a column. This setup is inherently cooperative, as agents must work together to achieve the correct positioning. I discuss several common frameworks in cooperative tasks, including independent learners, team learning with a shared Q-table, and centralized learning. This serves as an introduction to "infinite" agent learning.


### Exercises

After running the examples, try extending the codebase with one of the following exercises:

**Exercise 1: PettingZoo Integration**
- Incorporate [PettingZoo](https://pettingzoo.farama.org/index.html) environments into the simulation and verify the results using Deep Q-Learning with different training approaches (shared, independent, and centralized).

**Exercise 2: Drone Flocking Simulation**
- Create a custom environment that simulates a flock of drones that need to maintain connectivity. Consider the following aspects:
  - **Reward Function Design**: Develop an appropriate reward function that encourages flocking behavior by balancing cohesion (staying together) and separation (avoiding collisions).
  - **Action Space**: Discretise the action space into manageable options (e.g., 8 directional movements).
  - **Observation Space**: Define the optimal observation space for each drone. Consider options such as:
    - Absolute position of all drones
    - Relative positions of the N nearest neighboring drones
    - Local density and directional information
  - **Implementation**: Build both the environment and the agent implementations.
  - **Validation**: Verify that the agents successfully learn the desired policy to maintain connectivity and exhibit emergent flocking behavior.
