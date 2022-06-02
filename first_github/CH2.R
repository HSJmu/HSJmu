# packages
#install.packages("sf")
#install.packages("raster")

#install.packages("spData")
#install.packages("spDataLarge",
                # repos = "https://nowosad.github.io/drat/",
                # type = "source") #지리공간데이터샘플을내장

# 파일 경로 변경
#.libPaths()
#.libPaths("C:/Users/lg/Desktop/R/4.1")

######
###2.2 벡터 데이터########################################3

library(sf)
library(raster)
library(spData)
library(spDataLarge)
# install.packages("tidyverse")
library(tidyverse)


#Sys.setlocale("LC_ALL","English")
#vignette(package="sf")
#vignette("sf1") # 여기서도 에러뜸

world # 90개 나라에 대한 면적, 인구, 수명, GDP 등을 알 수ㅇ
names(world)
world$type
world$geom # 포인트좌표
# WGS84 : 저번 ppt에 있음, 전세계, gps시스템과 동일 원점 사용
summary(world["lifeExp"])
summary(world)

plot(world)  # 에러

world_mini <- world[1:2, 1:3] # 1~2행 / 1열~3열 에 해당하는 데이터만 가져오기
world_mini


library(sp)
world_sp <- as(world,Class = "Spatial")
class(world_sp) # sp data

# st_as_sf 함수,, sp -> sf로 전환
world_sf <- st_as_sf(world_sp)



plot(world[3:6])
plot(world["pop"])

# 다른 지도층을 추가하기
## 아시아만 뽑기
world_asia <- world[world$continent=="Asia",]
plot(world_asia)
plot(asia)

## st_union() 함수 : 설정 값들을 하나의 데이터로 합쳐줌
asia <- st_union(world_asia) # 아시아 국가 데이터를 하나로 합치기

asia
world_asia
world %>% View()

# 아시아만 빨간색으로 표시
plot(world["pop"],reset=FALSE) # reset=FALSE이면 지도 요소를 더 추가할 수 있는 모드로 플롯을 유지
plot(world_asia, add=TRUE, col="red")



# Base Plot arguments
## 대륙별 중심점에 원을 덮어 씌우기
## st_centroid() 함수 : 폴리곤의 중심점을 계산하는 함수
### of_largest = TRUE : 각각의 폴리곤의 가장 중앙점을 찾아줌, true로 안해줄 경우 전체 폴리곤의 중앙점을 찾아줌
                        # 각 나라별 중앙점들을 찾아줌, 전체 지도의 중앙점 찾아줌

plot(world["continent"],reset=FALSE)
cex=sqrt(world$pop) /10000 # 이 활동을 하지 않고 인구수를 토대로 중점의 크기를 설정하게끔 한다면 너무 큰 원이 나옴
                           # 표준화 너낌
world_cents=st_centroid(world,of_largest=TRUE) # 다각형(국가별) 중앙점 계산

## st_geometry()함수 : world_cents의 위치값만 뽑아줌, cex= : 인구의 크기에 따라 
plot(st_geometry(world_cents),add=TRUE,cex=cex)


# 특정 나라를 중심으로 확장하여 주변 나라 표시하기
india <- world[world$name_long == "India",]

plot(st_geometry(india),expandBB = c(0, 0.2, 0.1, 1),col="gray",lwd=3)
plot(world_asia[1],add=TRUE) # world_asia[0] : geometry에 대한 column



##
# sfg
# point : st_point()
# linestring : st_linestring()
# ...
st_point(c(5,2))    # XY point
st_point(c(5,2,3)) # 3차원  # XYZ point
st_point(c(5,2,1),dim="XYM") # XYM point
st_point(c(5,2,3,1)) # XYZM point

plot(st_point(c(5,2)))


# multipoint, linestring

# 매트릭스 사용
#multipoint
multipoint_matrix <- rbind(c(5,2),c(1,3),c(3,4),c(3,2))
st_multipoint(multipoint_matrix)

# linestring
linestring_matrix <- rbind(c(1,5),c(4,4),c(4,1),c(2,2),c(3,2))
st_linestring(linestring_matrix)



# list를 사용 : multilinestrings, polygons, and geometry collections
# polygon
polygon_list=list(rbind(c(1,5),c(2,2),c(4,1),c(4,4),c(1,5)))
st_polygon(polygon_list)
plot(st_polygon(polygon_list))

# polygon with a hole------------------------------
polygon_border <- rbind(c(1,5),c(2,2),c(4,1),c(4,4),c(1,5))
polygon_hole <- rbind(c(2,4),c(3,4),c(3,3),c(2,3),c(2,4))
polygon_with_hole_list <- list(polygon_border,polygon_hole)

st_polygon(polygon_with_hole_list)
plot(st_polygon(polygon_with_hole_list)) # 두개의 폴리곤

# multi linestring
multilinestring_list=list(rbind(c(1,5),c(4,4),c(4,1),c(2,2),c(3,2)),
                          rbind(c(1,2),c(2,4)))
st_multilinestring(multilinestring_list)
plot(st_multilinestring(multilinestring_list))


# multipolygon
multipolygon_list=list(list(rbind(c(1,5),c(2,2),c(4,1),c(4,4),c(1,5))),
                       list(rbind(c(0,2),c(1,2),c(1,3),c(0,3),c(0,2))))
st_multipolygon(multipolygon_list)
plot(st_multipolygon(multipolygon_list))

# geometrycollection
geometrycollection_list=list(st_multipoint(multipoint_matrix),
                            st_linestring(linestring_matrix))
st_geometrycollection(geometrycollection_list)
plot(st_geometrycollection(geometrycollection_list))



####### sfc : simple feature columns / 두 개의 지리적 특성을 하나의 칼럼 객체로 합침

## st_sfc()함수
# 1. 두개의 단순 지리 특성 기하 점을 한개의 단순 지리특성 칼럼 객체로 합치기
# sfc point
point1 <- st_point(c(5,2))
point2 <- st_point(c(1,3))
points_sfc <- st_sfc(point1,point2)
points_sfc

# 2. 두개의 단순 지리특성 기하 면을 st_sfc()함수로 한개의 단순 지리 특성 칼럼 객체로 합치기
## st_geometry_type() : 기하유형을 확인
# sfc polygon
polygon_list1 <- list(rbind(c(1,5),c(2,2),c(4,1),c(4,4),c(1,5)))
polygon1 <- st_polygon(polygon_list1)
polygon_list2 <- list(rbind(c(0,2),c(1,2),c(1,3),c(0,3),c(0,2)))
polygon2 <- st_polygon(polygon_list2)
polygon_sfc <- st_sfc(polygon1,polygon2)
polygon_sfc
plot(polygon_sfc)

# sfc MULTILINESTRING
multilinestring_list1 <- list(rbind(c(1,5),c(4,4),c(4,1),c(2,2),c(3,2)),
                              rbind(c(1,2),c(2,4)))
multilinestring1 <- st_multilinestring((multilinestring_list1))

multilinestring_list2 <- list(rbind(c(2,9),c(7,9),c(5,6),c(4,7),c(2,7)),
                              rbind(c(1,2),c(2,4)))
multilinestring2 <- st_multilinestring(multilinestring_list2)

multilinestring_sfc <- st_sfc(multilinestring1,multilinestring2)
multilinestring_sfc
st_geometry_type(multilinestring_sfc) # 기하 유형 확인
plot(multilinestring_sfc)

# sfc GEOMETRY
point_multilinestring_sfc <- st_sfc(point1,multilinestring1)
st_geometry_type(point_multilinestring_sfc)


# -----------------------------# 

# 2022.03.17 
points_sfc

## st_crs()함수 : crs에 대한 정보 추출해줌 
st_crs(points_sfc)

# crs 지정하기 위해 1. epsg(SRID) 또는 2. proj4string 속성을 사용할 수 있음


# 1. EPSG 정의 / 짤방서 
points_sfc_wgs <- st_sfc(point1, point2, crs = 4326)
st_crs(points_sfc_wgs)

# 2.proj4string 정의
st_sfc(point1,point2,crs = "+proj = longlat + datum = WGS84 + no_defs")





#### sf class
# 위의 위치데이터에 속성데이터(이름, 특정값 그룹 등)를 추가
lnd_point <- st_point(c(0.1,51.5))
lnd_geom <- st_sfc(lnd_point,crs = 4326)
lnd_attrib <- data.frame(
  name="London",
  temperature = 25,
  date=as.Date("2017-06-21")
)

lnd_sf <- st_sf(lnd_attrib,geometry = lnd_geom)
lnd_sf # 위치정보와 속성정보 전부 가질 수 있게 해줌.
plot(lnd_sf)

#
## 2.3 Raster data
#install.packages("rgdal") # 공간정보분석에 유용한 함수가 많은 패키지임
library(rgdal)

raster_filepath <- system.file("raster/srtm.tif",package = "spDataLarge")
new_raster <- raster(raster_filepath)

dim(new_raster) # 몇행 몇열 몇개
ncell(new_raster) # 전체 셀의 개수(위의 값 곱한거(dim))
extent(new_raster) # 
crs(new_raster) # 좌표계 시스템
inMemory(new_raster) # 메모리에 이 데이터가 저장 되어있나 아닌가

plot(new_raster) # 그림 표현

##-------------------------------
# 2022-03-22

###### 1. rasyer layer class

raster_filepath <- system.file("raster/srtm.tif",package = "spDataLarge")
new_raster <- raster(raster_filepath)
projection(new_raster)
new_raster
new_raster3 <- new_raster
projection(new_raster3)

# number of layers
nlayers(new_raster)


my_raster <- raster(nrows = 8, ncols = 8, res= 0.5,  # res : 해상도
                    xmn=-2.0, xmx=2.0, ymn=-2.0, ymx=2.0, vals = 1:64)
# 시각화
plot(my_raster,main="my raster(64cells = 8rows * 8cols")
####### 2. rasterbrick class
multi_raster_file=system.file("raster/landsat.tif",package = "spDataLarge")
r_brick <- brick(multi_raster_file)

r_brick

plot(r_brick)

nlayers(r_brick)


###### 3. rasterstack class
raster_on_disk <- raster(r_brick, layer= 1)
raster_in_memory <- raster(xmn = 301905, xmx = 335745,
                           ymn = 4111245, ymx = 4154085,
                           res = 30)
values(raster_in_memory) <- sample(seq_len(ncell(raster_in_memory)))
crs(raster_in_memory) <- crs(raster_on_disk)

r_stack <- stack(raster_in_memory, raster_on_disk)
r_stack

plot(r_stack)



###############
################ 2.4 coordinate Reference Systems
# CRS
library(sf)
vector_filepath <- system.file("vector/zion.ggkg",package = "spDataLarge")
new_vector <- st_read(vector_filepath)

new_vector_2 <- st_set_crs(new_vector, 4326)

raster_filepath <- system.file("raster/srtm.tif", package ="spDataLarge")
new_raster(raster_filepath)
projection(new_raster)

new_raster3 <- new_raster
projection(new_raster3) = "+proj=longlat +datum=WGS84 +no_defs"


# Unit ( 단위 )


################################################################
#-------------------------------------------------------------#
# CH3. Attribute data operations
library(sf)
library(raster)
library(spData)
library(spDataLarge)
library(tidyverse)
library(stringr)
library(tidyr)

methods(class="sf")


