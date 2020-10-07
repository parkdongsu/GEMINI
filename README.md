# GEMINI
Gemini means 'GEneral exaMINing and visualizing application for paired Institution'.
It uses a connection of DB for extract a part of CDM data and makes rds files where you want.

# Branch Description
이 브랜치는 Docker를 이용하여 간편히 RDS 생성을 하기 위해 생성된 브랜치입니다 <br/>
아래와 순서로 진행하시면 됩니다.

# HOW TO USE
1. 도커파일을 이미지로 변환한다
	- docker build -t [이미지 이름:태그] .
	- e.g.) docker build -t gemini:1.0 .
2. RDS를 받을 볼륨을 생성한다
	- docker volume create [볼륨 이름]
	- e.g.) docker volume create gemini-vol
3. 컨테이너를 생성하여 RDS를 추출한다(현재 DBMS는 postgre SQL 고정)
	- docker run -it --rm -e CDM_URL=[CDM 서버 주소] -e CDM_USER=[DB 사용자 ID] -e CDM_PW=[DB 사용자 PW] -e CDM_DATABASE=[CDM이름] -e CDM_SCHEMA=[스키마] -v [볼륨 이름]:/root/gemini [이미지 이름:태그]
	- e.g.) docker run -it --rm -e CDM_URL=111.111.111.111 -e CDM_USER=admin -e CDM_PW=ajouadmin -e CDM_DATABASE=cdmv6 CDM_SCHEMA=dbo -v gemini-vol:/root/gemini gemini:1.0
4. 도커 볼륨 정보 확인
	- Docker volume inspect [볼륨이름]
	- Docker volume inspect gemini-vol
5. 볼륨 위치 접근
	- `"Mountpoint": "/var/lib/docker/volumes/gemini-vol/_data"`와 같이 마운트 위치 찾기
	- cd [Mountpoint]
	- cd /var/lib/docker/volumes/test-vol/_data/Gemini RDS/[CDM 이름]/에 있는 정보 압축하기
	

# Poster
![Poster](/OHDSI_GEMINI_poster.png)
