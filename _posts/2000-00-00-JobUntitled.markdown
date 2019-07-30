---
title:  "VR을 이용한 지능형 가상면접 시스템"
subtitle: "Java"
author: "sejin"
avatar: "img/authors/jin.png"
image: "img/JU.png"
date:   1011-10-01 01:10:01
---

## ScreenShot

![ex](../img/JU.gif)


- - -
### 개발인원/기간 : 11명 / 7달 

### 개발내용

1. 자기소개서를 웹에 제출
2. 제출한 자기소개서를 바탕으로 키워드 추출 및 성향분석 실행
3. 추출한 키워드를 통해서 질문을 구성
4. 질문 구성이 완료되면 사용자에게 앱에서 알림
5. 이후 사용자는 면접 부스로 이동
6. 부스에서 VR통하여 면접을 본 후 웹에서 결과로 피드백

의 순서를 가지고 있다.

서버로는 AWS EC2 Ubuntu DB는 Maria DB를 사용하였다.
- - -
### 맡은역할

로컬팀에서 JavaFX를 사용하여 데스크탑 응용프로그램을 제작하였고 면접자의 음성을 TEXT로 바꾸어주는 STT라이브러리를 사용하여 서버로 보내주는모듈을 맡았다.

각각의 모듈들이 MultiThread 환경에서 원하는대로 동작하게 하기위해 Thread Pool을 공부하는 계기가 되었고 결국 동작하게끔 구현하였다.

그외 타 외부 VR영상 플레이어를 자바환경에서 핸들링하기위해 AHK를 사용하였다. 

STT(Speech To Text)를 사용하기위해 여러가지 api 서비스들을 찾아보았으나 결과적으로 인식률과 API비용등을 고려해볼때 IBM Watson과 Google의 api중 고민하다가 Google의 STT API를 사용하였다.

애초 개발처음 단계에서는 실시간으로 음성변환을 하지않아도 되었으나 개발 중간 화면을 보는 제 3 자의 입장을 고려하여 실시간 STT를 구현하여하였다.

아쉽게도 Google STT를 설명해주는 Docs에는 Streaming코드가 없어 한참을 찾은 후 적용할 수 있었다.

(이 글을 쓰면서 다시 api문서를 찾아보는데 2018년도 12월에 InfiniteStreamRecognize 공식 문서가 업데이트되었다. 아..)



- - -

### 구성도
![ex](../img/structure.jpg)
![ex](../img/coex.jpg)

### ETC
사용한 툴 : Visual Studio Code, IntelliJ, Dbeaver, SourceTree, Scene Builder, Slack, Redmine

2018.06 캠퍼스 CEO 경진대회 은상
2018.10 졸업작품 코엑스 전시 및 교내 졸업작품 우수상 
