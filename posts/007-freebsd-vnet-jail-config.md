---
title: FreeBSD VNET Jail 구성 방법
published: 2024-09-25T15:27:41+0900
updated: 2024-10-02T18:47:00+0900
---

FreeBSD의 VNET Jail이 무엇인지, 어디에 쓰는지 소개한다. 구성 과정에서 마주칠 수
있는 문제와 해결 방안을 다룬다.

## FreeBSD Jail

BSD 계열 운영체제는 **Jail** 기능이 있는데, 프로세스, 파일 시스템, 네트워크,
사용자 및 권한을 격리하는 환경을 제공한다.

흔히 Linux의 **Docker**와 비교되지만, Docker와 달리 Jail은 운영체제의 커널
레벨에서 지원하는 기능이다. 따라서 운영체제와 밀접히 통합되어 있으며, 보안 및
안정성, 자원 관리, 네트워크 분리, ZFS 파일 시스템과의 상성과 같은 강력한 장점이
있다.

사실, 요즘의 Docker는 개발 편의를 위한 도구로써 의미가 있다고 본다. Docker
이미지의 생성 및 레포지토리를 통한 배포 등 개발 환경의 구축을 간소화하고 편의를
제공해주는 도구라는 측면에서 장점이 분명하다. 하지만 모든 컨테이너가 하나의
docker daemon에 의해 관리되고, 이 프로세스가 root 권한으로 실행된다는 점에서
근본적인 보안 문제가 있다. 이에 보안 우려가 중요한 환경에서는 **Podman**과 같은
대체제 또는 분산 환경에 적합한 **Kubernetes**를 사용하는 경우가 많다.

Jail 또한 Docker의 편의성에는 못 미친다. 다만 Docker는 개발 도구로, Jail은
서비스를 실제로 운영하는 환경으로 적합하다.

> FreeBSD Jail에 대한 자세한 내용은 추후 포스팅하겠다.

## VNET Jail

> A FreeBSD VNET jail is a virtualized environment that allows for the
> isolation and control of network resources for processes running within it.
> It provides a high level of network segmentation and security by creating a
> separate network stack for processes within the jail, ensuring that network
> traffic within the jail is isolated from the host system and other jails.
> \
> – [FreeBSD Handbook][handbook-vnet], **17.2.4. VNET Jails.**

[handbook-vnet]: https://docs.freebsd.org/en/books/handbook/jails/#vnet-jails

**VNET Jail**은 호스트와 별개의 [네트워크 스택][network-stack]을 가진다.
네트워크 스택이란 통신 프로토콜의 소프트웨어 구현체라는데, 솔직히 네트워크 쪽은
아직 용어와 개념이 확실하게 잡히진 않아서 잘 모르겠다. 아무튼 그 구현체가
다르다는 사실은 VNET Jail에서 네트워크 인터페이스와 라우팅 테이블이 호스트와
완전히 독립되어 있다는 점에서 맥락을 이해할 수 있다.

[network-stack]: https://en.wikipedia.org/wiki/Protocol_stack

### Non-VNET Jail과 비교

네트워크 스택이 호스트로부터 독립되어 있다는 점에서, VNET Jail은 다음과 같은
특징을 갖는다.

1. **네트워크 인터페이스가 호스트와 다르다.** 따라서 호스트와 다른 서브넷의
   ip를 할당받을 수 있다. Non-VNET Jail는 ip aliasing을 이용하여 호스트와 같은
   서브넷 내에서만 ip를 할당받는다.
2. **라우팅 테이블이 호스트와 분리된다.** Non-VNET Jail에서는 policy-based
   routing으로 라우팅을 분리할 수 있지만, VNET Jail에서는 호스트와 동등한
   별개의 머신처럼 라우팅을 관리할 수 있다.
3. **방화벽 규칙을 개별적으로 설정할 수 있다.** Non-VNET Jail은 호스트의 방화벽
   규칙을 따르지만, VNET Jail은 각 Jail을 별개의 머신으로 보고 독립적인 방화벽
   규칙을 적용할 수 있다.
4. **관리 측면에서 호스트와 결합도가 낮아 관리가 용이하다.** 위에서 언급한
   policy-based routing와 같이 non-VNET Jail에서도 동일한 목적을 달성할 수는
   있어도, VNET Jail은 각 Jail을 호스트와 동등한 별개의 머신처럼 관리할 수
   있기에 일종의 모듈화와 같은 효과가 있다.

### VNET Jail의 활용

운용 예시를 살펴보면 VNET Jail의 매력을 이해하는 데 도움이 된다. VNET Jail은
호스트와 다른 네트워크 인터페이스를 가지므로 각 Jail을 서로 다른 VLAN에 둘 수
있다. 또한 라우팅 테이블이 호스트와 다르므로 일부 Jail을 특정한 VPN에
연결하도록 구성할 수 있다.

개인적으로 VNET Jail을 활용하여 호스트와 각 Jail에 고유한 IP를 부여하여 다양한
웹 서비스를 운영하고 있다. 또한, **dnsmasq**로 로컬 DNS 서버를 구축해두고 각
서비스에 서로 다른 로컬 도메인 이름을 매핑해 두었다. 하나의 reverse proxy
아래에 모든 서비스를 두는 방법도 있었지만, 이렇게 네트워크를 구성한 이유는 각
Jail을 물리적으로 구분되는 별도의 서버로 가정해 운영해보고 싶었기 때문이다.

또한 VLAN을 나누어, 각 Jail을 서로 다른 VLAN에 배치하는 구성도 실험하고자 했다.
VNET Jail은 호스트 및 다른 VNET Jail들과 각각 독립된 네트워크 인터페이스를
가지기 때문에, 각 Jail을 서로 다른 VLAN에 두면 이들 간의 통신은 반드시 라우터를
거쳐야만 이루어질 수 있다. 이러한 구성을 통해 각 VLAN에 각각 다른 방화벽 정책을
설정하는 등의 실험을 진행할 수 있었다.

> 홈 네트워크 구성 또한 나중에 기회가 되면 포스팅하겠다.

### VNET Jail 구성 방법

VNET Jail은 epair 인터페이스를 생성하여 한 쪽은 브릿지에, 다른 쪽은 Jail에
연결하도록 구성한다.

다음 내용을 `/etc/rc.conf`에 추가한다.

```unix
cloned_interface="bridge0"

ifconfig_bridge0="addm em0 up"
```

> **Note.** `em0`와 같은 인터페이스 이름은 기기마다 다를 수 있으니 그대로
> 사용할 수 없다.

다음 내용을 `/etc/jail.conf`에 추가한다.

```unix
my-vnet-jail {
# ...

# VNET/VIMAGE
  vnet;
  vnet.interface = "${epair}b";

# NETWORKS/INTERFACES
  $id = "154";
  $ip = "192.168.1.${id}/24";
  $gateway = "192.168.1.1";
  $bridge = "bridge0";
  $epair = "epair${id}";

# ADD TO bridge INTERFACE
  exec.prestart  = "/sbin/ifconfig ${epair} create up";
  exec.prestart += "/sbin/ifconfig ${epair}a up descr jail:${name}";
  exec.prestart += "/sbin/ifconfig ${bridge} addm ${epair}a up";
  exec.start    += "/sbin/ifconfig ${epair}b ${ip} up";
  exec.start    += "/sbin/route add default ${gateway}";
  exec.poststop = "/sbin/ifconfig ${bridge} deletem ${epair}a";
  exec.poststop += "/sbin/ifconfig ${epair}a destroy";
}
```

자세한 내용은 [FreeBSD handbook][handbook-creating-vnet]를
참조한다.

[handbook-creating-vnet]: https://docs.freebsd.org/en/books/handbook/jails/#creating-vnet-jail

#### 문제점

간혹 Jail을 호스트에서 제거[^notation-remove]한 이후에도 `epair###b`
인터페이스가 Jail에서 release되지 않아서 호스트에서 보이지 않는
문제[^not-releasing-if]가 발생한다. 원래 `jail.conf`[^manpage-jail-conf]에서
`vnet.interface`의 인수로 설정한 인터페이스는 자동으로 release되어야
한다.[^manpage-jail]

[^notation-remove]:
    Jail의 **생성(create)/제거(remove)**라는 표현은 Jail을
    구성하는 **userland**의 생성/제거와 독립적이므로 주의해야 한다. Docker에
    익숙한 경우, Jail의 생성/제거는 Docker 컨테이너의
    생성(create)/제거(rm)보다는 **시작(start)/정지(stop)**와 더 비슷하다고
    이해할 수 있다.

[^not-releasing-if]: 해당 문제 보고는 [FreeBSD 포럼][forum-1] 참조.

[^manpage-jail-conf]: `man 5 jail.conf` 참조.

[^manpage-jail]:
    `man 8 jail` 참조:

    ```
    vnet.interface
            A network interface to give to a vnet-enabled jail after is it
            created.  The interface will automatically be released when the
            jail is removed.
    ```

[forum-1]: https://forums.FreeBSD.org/threads/interface-does-not-return-to-host-after-kill-jail.92730/post-648334

#### 해결 방법

Jail이 제거되는 시점에 호스트에서 `ifconfig -vnet` 명령어로 수동으로
인터페이스를 release해줄 수 있다. `jail.conf`의 `exec.prestop` 인수에 다음과
같이 명령어를 추가하면 된다.

```unix
my-vnet-jail {
# ...

# ADD TO bridge INTERFACE
  exec.prestart  = "/sbin/ifconfig ${epair} create up";
  exec.prestart += "/sbin/ifconfig ${epair}a up descr jail:${name}";
  exec.prestart += "/sbin/ifconfig ${bridge} addm ${epair}a up";
  exec.start    += "/sbin/ifconfig ${epair}b ${ip} up";
  exec.start    += "/sbin/route add default ${gateway}";

  # Add this line
  exec.prestop  += "/sbin/ifconfig ${epair}b || /sbin/ifconfig ${epair}b -vnet $name";

  exec.poststop = "/sbin/ifconfig ${bridge} deletem ${epair}a";
  exec.poststop += "/sbin/ifconfig ${epair}a destroy";
}
```

- `exec.stop`은 Jail 제거 시점에 Jail 내부에서 실행된다.
- `exec.prestop`과 `exec.poststop`은 각각 Jail 제거 직전과 직후에 호스트에서
  실행된다.

## Jail 구축 관련 참고 사항

### DHCP를 이용한 IP 할당

위에서 소개한 방법은 각 Jail의 IP를 수동으로 할당한다. DHCP를 이용하여 자동
할당받고자 하는 경우, 각 Jail에서 DHCP 클라이언트를 따로 구성해줘야 한다. 또한
`jail.conf`에서 각 Jail에 대해 permission을 추가로 부여할 필요가 있던 것으로
기억하는데, 정확한 방법은 기억나지 않는다.

### PostgreSQL 등 DB 구축

VNET Jail과 관련은 없으나, PostgreSQL과 같은 일부 데이터베이스[^other-dbs]를
Jail에서 정상적으로 구동하기 위해서는 해당 Jail에 별도의 제약을 해제해야 한다.
Jail을 소개할 때 언급했듯이, Jail은 각각 자원의 할당 및 권한 관리를 세세히
설정할 수 있다.

PostgreSQL의 경우, `sysvipc`[^manpage-jail-2] 제약을 해제해야 한다. `jail.conf`
파일에 다음과 같이 추가한다.

```
my-postgres-jail {
# ...

# PERMISSIONS
  allow.sysvipc;
}
```

[^other-dbs]:
    경량의 파일 기반 데이터베이스인 SQLite는 따로 권한을 요구하지
    않았다.

[^manpage-jail-2]:
    `man 8 jail` 참조:

    ```
    allow.sysvipc
            A process within the jail has access to System V IPC
            primitives.  This is deprecated in favor of the per-
            module parameters (see below).  When this parameter is
            set, it is equivalent to setting sysvmsg, sysvsem, and
            sysvshm all to “inherit”.
    ```
