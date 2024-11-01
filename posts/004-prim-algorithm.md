---
title: Prim 알고리즘
katex: on
published: 2024-09-18T18:00:00+09:00
---

> PS(Problem Solving) 문제 풀이를 위해 작성한 글입니다. 이론적인 설명은 배제했습니다.

## 개요

Prim 알고리즘은 [minimum spanning tree](/posts/1/minimum-spanning-tree)를 찾는 그리디 알고리즘이다. 임의의 시작 정점에서부터 트리를 성장시키며 MST를 찾으며, priority queue의 이점을 활용한다.

## 알고리즘

프림 알고리즘은 임의의 시작 정점에서 시작하여 트리를 확장해나가는데, 트리에 인접한 간선 중 최소 비용인 것을 선택한다. 트리에 인접한 최소 가중치 간선을 찾기 위해 **연결 비용**이라는 개념을 도입한다.

연결 비용은 각 정점에 대해 정의되며, 해당 정점에서 트리와 연결될 수 있는 간선들 중 가장 작은 가중치로 결정된다. 만약 트리와 연결되는 간선이 존재하지 않으면, 연결 비용은 ∞로 설정된다. 트리에 인접한 간선들 중 연결 비용이 가장 작은 정점과 간선을 선택함으로써 최소 가중치의 간선을 찾을 수 있다.

연결 비용을 매번 계산하면 비효율적이므로, 미리 계산해두고 필요시 갱신한다. 구체적으로는 새로운 정점이 트리에 추가되었을 때, 그 정점과 연결된 모든 인접 정점들에 대해 연결 비용을 다시 계산한다.

연결 비용을 다시 계산할 때도 모든 간선의 가중치를 비교할 필요는 없다. 트리에 추가된 정점 $v$와 그 인접 정점 $w$에 대해, $w$의 새로운 연결 비용은 기존 연결 비용과 간선 $vw$의 가중치 중 작은 값이다.

## 자료구조

연결 비용은 priority queue로 관리된다. priority queue는 데이터가 항상 정렬된 상태로 유지되며, 데이터의 삽입과 추출이 빈번한 상황에서 효율적이다. 연결 비용은 자주 갱신되며, 최소값을 반복적으로 검색해야 하므로 priority queue를 사용하는 것이 적합하다.

구체적으로, priority queue는 **(정점, 연결 비용, 간선)**[^1]으로 구성된 튜플을 관리한다. 이때 연결 비용이 가장 낮은 튜플이 우선적으로 선택된다. 또한, 연결 비용이나 간선이 갱신될 때마다 새로운 튜플을 priority queue에 삽입한다.

연결 비용과 간선을 직접 수정하지 않고 새로운 튜플을 추가하는 이유는 priority queue에서 이미 삽입된 데이터를 수정하기가 어렵기 때문이다. 또한, 연결 비용은 항상 감소하는 방향으로만 갱신되므로, 새로 삽입된 튜플이 기존의 튜플보다 높은 우선순위를 갖는 것이 보장된다. 추가로, 이전에 삽입된 outdated된 튜플을 무시하기 위해 별도의 lookup list를 사용하여 보완한다.

[^1]: 실제로는 **(정점, 연결 비용, 다른 정점)**으로 충분하다. 간선이 이미 정점과 연결 비용(간선 가중치)를 포함하고 있기 때문이다. 이 튜플은 간선과 1:1 대응된다.

## 구현

> 구현은 BOJ 문제 답안을 참고한다.
 
- [\[BOJ\] #1197 최소 스패닝 트리](/posts/5/boj-1197#prim)

## 기타

### Kruskal vs Prim

Kruskal 알고리즘 또한 MST를 찾는 알고리즘이다. Kruskal은 희소 그래프에서, Prim은 밀집 그래프에서 효율적이라고 알려져 있다. 이는 시간복잡도와도 관련이 있지만 그보다도 Kruskal 알고리즘에 쓰이는 union-find 자료구조가 Prim 알고리즘에서 쓰이는 priority queue보다 overhead가 적기 때문인 것으로 보인다.

시간복잡도만으로는 Kruskal 알고리즘이 희소 그래프에서 효율적인 이유를 설명하기 어려울 수 있다. 두 알고리즘의 시간복잡도는 Kruskal 알고리즘이 $\text{O}(|E|\log{|E|})$, Prim 알고리즘을 이진 힙으로 구현 시 $\text{O}(|E|\log{|V|})$ 이다. 정점 개수가 $|V|$인 트리의 간선 수는 $|V-1|$이므로 임의의 연결 그래프는 $|E| \geq |V|-1$이기 때문에, 희소 그래프라 하더라도 대부분의 연결 그래프는 $|E| \gt |V|$이다. $|E|\log{|E|} > |E|\log{|V|}$ 이므로 Prim 알고리즘이 Kruskal 알고리즘보다 빨라야 한다는 결론이 도출된다.

이는 시간 복잡도만으로 알고리즘의 속도를 평가할 수 없음을 시사한다. Big-O 표기법에서 상수 계수 등은 생략하기 때문이다.

## 참조

- [프린스턴 대학 알고리즘 강의 - Minimum Spanning
Trees](https://algs4.cs.princeton.edu/43mst/)
- [Wikipedia - Prim's algorithm](https://en.wikipedia.org/wiki/Prim%27s_algorithm)