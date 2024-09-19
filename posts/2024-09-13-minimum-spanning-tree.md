---
title: Minimum spanning tree (MST, 최소신장트리)
---

> PS(Problem Solving) 문제 풀이를 위해 작성한 글입니다. 이론적인 설명은
> 배제했습니다.

## TL;DR

- Kruskal 알고리즘은 MST를 Greedy하게 찾는 알고리즘이다.
- cycle이 형성되지 않도록 최소 비용 간선을 선택해나간다.
- cycle이 형성되었는지 알기 위해 union-find를 사용한다.

## Minimum spanning tree

최소신장트리(MST, Minimum spanning tree)는 그래프의 **모든 정점을 포함하는
subgraph 중 weight의 합이 최소인 트리**이다. 즉, MST는 그래프의 모든 정점을
최소비용으로 연결하는 트리다.

우선, 몇 가지 가정을 하겠다.

- Connected graph에서 논한다.
- 간선 가중치가 **음수, 0**일 수 있다.

간선 가중치가 모두 다른 값을 가지면 MST가 유일하고, 그 역도 성립한다. 즉,
가중치가 같은 간선들이 있으면 MST를 여러 개 가질 수 있다. 

## Kruskal's algorithm
 
MST를 greedy하게 찾을 수 있다. **cycle이 형성되지 않도록** 주의하면서 **최소
비용 간선을 선택**해나간다. 그래프 이론의 cut property에 의해 최적해가 보장된다.

그래프에 간선을 추가했을 때 cycle이 형성되려면 양 끝 정점이 동일한 컴포넌트에
속하면 되는데, 이를 판별하기 위하여
**[union-find](/posts/2024-09-14-union-find)** 자료구조를 활용한다.

Union-find는 disjoint set들을 나타내는 자료구조다. 그래프의 각 컴포넌트는
union-find에서 disjoint set으로 표현되며, 컴포넌트를 간선으로 연결하려면 먼저
간선을 선택하고 `union` 연산으로 disjoint set들을 합쳐주어야 한다.

구현 방법을 세부적으로 설명하면 다음과 같다.

1. 그래프의 모든 컴포넌트가 하나의 정점만을 가지도록 초기화한다.
2. 가중치가 작은 간선부터 순회한다. 양 끝 정점이 서로 연결되어있는지
확인한다. 연결되지 않았으면, 두 정점이 속하는 컴포넌트를 병합하고 간선은
저장해둔다.
    - 정점이 속한 컴포넌트를 알아내기 위해 `find` 연산을 사용한다.
    - 두 컴포넌트를 연결하기 위해 간선을 선택한 이후 `union` 연산을 사용한다.
3. 간선을 모두 방문한 이후, 저장해둔 간선들은 MST를 이룬다.


의사코드는 다음과 같다.

```
function Kruskal(G = (V, E))
    X := {}
    for each vertex u ∈ V
        MAKESET(u)
    for each {u, v} ∈ E ordered by weight increasing
        if FIND(u) ≠ FIND(v)
            add {u, v} to X
            UNION(u, v)
    T := (V, X)
    return T
```

## Reference

- [프린스턴 대학 알고리즘 강의 - Minimum Spanning Trees](https://algs4.cs.princeton.edu/43mst/)
- [블로그 1](https://rntlqvnf.github.io/lecture%20notes/algorithm-5th-week-1/)
