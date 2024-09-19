---
title: Kruskal's algorithm
---

> PS(Problem Solving) 문제 풀이를 위해 작성한 글입니다. 이론적인 설명은
> 배제했습니다.

## TL;DR

- Kruskal 알고리즘은 MST를 greedy하게 찾는 알고리즘이다.
- 사이클이 형성되지 않도록 최소 비용 간선을 선택해나간다.
- 사이클 형성 여부를 판단하기 위해 union-find를 사용한다.

## Introduction
 
Kruskal 알고리즘은 [minimum spanning
tree](/posts/2024-09-13-minimum-spanning-tree)를 찾는 알고리즘이다. 핵심
아이디어는 매 순간 **최소 비용 간선을 선택**하면서, **사이클이 형성되지 않도록** 주의하는 것이다.

매번 최소 비용 간선을 고른다는 점에서 알고리즘이 greedy함을 알 수 있다. 이런
접근이 가능한 것은 그래프 이론의 cut property에서 기인한다. 증명 과정은
생략하겠다.

사이클을 피하려면 같은 컴포넌트에 속하는 정점 사이에는 간선을 추가하면 안 된다.
이 문제를 다루기 위해 **union-find** 자료구조를 사용한다. Union-find는 그래프에서
정점과 컴포넌트의 소속 관계를 관리하는 데 유용하다.

## Union-find

[Union-find](/posts/2024-09-14-union-find)는 disjoint set[^2]의 collection을 나타내는 자료구조다. 다음 3가지 연산을 지원한다.

- `makeset(x)`: x를 유일한 원소로 두는 새로운 집합 생성. 초기화를 위해 사용.
- `find(x)`: x가 속한 집합을 반환
- `union(x, y)`: x, y가 속한 두 집합을 병합 

그래프의 각 컴포넌트는 하나의 disjoint set으로 볼 수 있다. 두 정점이 서로 다른
컴포넌트에 있는지 확인할 때는 `find` 연산을 사용하고, 간선 {u, v}[^3]를 선택한
이후 두 컴포넌트를 병합하기 위해 `union(u, v)` 연산을 사용할 수 있다.

> **Note.** Union-find 자료구조는 그래프를 완전히 표현하지 않는다. 정점과
> 컴포넌트의 소속 관계만 다룰 뿐, 간선 정보는 직접적으로 관리하지 않는다.

[^2]: 서로소 집합. 공통 원소가 없는 두 집합을 말한다.
[^3]: u, v를 양 끝 정점으로 하는 간선

## Implementation

위에서 논의한 내용을 정리하면 다음과 같다.

- 그래프의 모든 컴포넌트가 하나의 정점만을 가지도록 초기화한다.
- 가중치가 작은 간선부터 순회한다.
    - 양 끝 정점이 서로 연결되어있는지 확인한다.
    - 연결되지 않았으면, 두 정점이 속하는 컴포넌트를 병합하고 간선은
저장해둔다.
- 간선을 모두 방문한 이후, 저장해둔 간선들은 MST를 이룬다.


다음은 알고리즘의 의사코드다.

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

> **Note.** Union-find의 구현은 [해당 문서][union-find-impl] 를 참조한다.

[union-find-impl]: /posts/2024-09-14-union-find#implementation

## Reference

- [프린스턴 대학 알고리즘 강의 - Minimum Spanning Trees](https://algs4.cs.princeton.edu/43mst/)
- [블로그 1](https://rntlqvnf.github.io/lecture%20notes/algorithm-5th-week-1/)
