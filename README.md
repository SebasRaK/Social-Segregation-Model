# Agent-Based Social Segregation Model

This project presents an agent-based simulation of social segregation dynamics implemented in Fortran.

The model studies how local interaction rules and tolerance thresholds can lead to large-scale segregation patterns within a population.

## Objectives

- Develop an agent-based model to study social interactions in a migratory context.
- Analyze how the perception toward immigrants evolves within a city.
- Define individual characteristics governing agent interactions.
- Establish interaction rules based on agent attributes.
- Study the temporal evolution of perception toward immigrants.
- Simulate segregation and collective behavior patterns emerging from local interactions.

## Methodology

Using the Agent-Based Modeling (ABM) approach, we simulated the dynamics generated when individuals from different countries arrive in a new society and developed a model to study how the perception toward immigrants evolves as social interactions emerge.

To achieve this, interaction rules between individuals were modeled according to a set of defined characteristics: economic level, education level, language proficiency, interests, and perception.

## Tools and Technologies

- Fortran
- Numerical simulations
- Agent-based modeling
- Gnuplot

## Results

An individual and global analysis of the evolution of agents’ perception throughout the simulation iterations was carried out.

An agent map is presented to illustrate the evolution of the entire city by analyzing each characteristic independently and, finally, all characteristics simultaneously.

Additionally, the evolution of the average global perception toward immigrants from a specific country is presented. Each characteristic is first analyzed separately and subsequently studied in combination with the remaining characteristics.

The following table explains the meaning of each image.

## Image Descriptions

| Image Name | Description |
|---|---|
| Economic Level Agent Map | Agent map showing the average perception of neighboring agents toward an immigrant at three different simulation stages: iteration 1, iteration \(2 \times 10^7\), and iteration \(1 \times 10^8\), from left to right. (a) Agents from the wealthy country. (b) Agents from the poor country. |
| Interests Agent Map | Agent map showing the evolution of perception considering only the interests characteristic. |
| Perception Agent Map | Agent map showing the evolution of perception considering only the perception characteristic. |
| Complete Characteristics Agent Map | Agent map showing the evolution of perception considering all agent characteristics simultaneously. |
| Average Perception — Economic Level | Average perception of city residents toward immigrants from different countries considering only the economic level characteristic. |
| Average Perception — Interests | Average perception of city residents considering only the interests characteristic. |
| Average Perception — Perception | Average perception of city residents considering only the perception characteristic. |
| Average Perception — All Characteristics | Average perception of city residents considering all characteristics simultaneously. |

## Compilation and Execution

```bash
gfortran social-segregation-model.f90 -o simulation
./simulation
```

## Conclusion

Using the Agent-Based Modeling (ABM) approach and interaction rules inspired by Robert Axelrod, we developed a model capable of reproducing several behaviors associated with migration dynamics.

The simulations suggest that immigrants from countries with different socioeconomic conditions may experience significantly different social perceptions within the city. While some groups tend to be positively perceived and integrate more easily, others experience less favorable perceptions during the interaction process.

Nevertheless, the results also show that positive local interactions can facilitate adaptation and social integration over time.
