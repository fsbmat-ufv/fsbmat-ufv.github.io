---
title:  "JavaFX를 이용한 상용구프로그램"
subtitle: "Java"
author: "sejin"
avatar: "img/authors/jin.png"
image: "img/QuickType_java.png"
date:   1011-10-01 01:10:02
---

## ScreenShot

![ex](../img/QuickType(java).gif)


- - -
### 개발인원/기간 : 1명 / 5일 

### 개발내역

고등학교때 제작하였던 <a href="http://127.0.0.1:4000/#/2015/04/25/QuickType_ahk" target="_blank">상용구프로그램</a>을 자바로 제작하였다.

그때 사용했었던 AHK를 사용하지않고 자바를 사용하여 다른 OS에서도 같은 기능을 하는 프로그램을 만들어보고 싶었고 고등학생때 만들었던 프로그램을 다시 만든다는것에 뜻을 가지고 다른언어로 만들어보았다. 

<a href="https://www.autohotkey.com/" target="_blank">AHK</a>와는 다르게 다양한 OS에서 사용하능하게 하기 위해 자바를 사용하였다.
네이티브 환경에 접근하기 위해 글로벌후킹을 필요로하여 <a href="https://github.com/kwhat/jnativehook" target="_blank">jnativehook</a>라이브러리를 사용하였다. 

Swing과 JavaFX중 고민하던중 최근 Swing을 사용하지 않았던 관계로 javaFX를 사용하여 제작하였다. 

CTRL + N(숫자) 를 입력하면 미리 작성하였던 문구들을 불러와 사용할 수 있다.
- - -

### ETC

사용한 툴 : IntelliJ, Scene Builder
<br>
<a href="https://github.com/cadinz/QuickType" target="_blank">SOURCE</a>
