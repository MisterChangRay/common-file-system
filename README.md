# common-file-system
一个独立的文件系统；提供小文件管理(30MB);便于分布式部署以提升性能;

##### 已完成功能
- 提供统一的文件管理方式;易于项目扩展和维护
- 提供文件上传/打包下载/图片缩略图等功能
- 可以使用独立的登录中心进行权限验证
- 提供图形化管理界面;方便使用

##### 快速开始
- clone项目到本地
- 导入数据库并配置 `jdbc.properties` 文件中的数据库连接信息;
- 引入maven配置中的依赖文件
- 直接使用或重构项目


##### 目前数据库信息,详细信息见"/resources/archives"目录下
```sql
drop database common_file_system;

create database common_file_system;

create table common_authorize_code;

create table common_file;
```

##### 可在`config.properties`中配置以下属性
- 允许上传的文件大小
- 允许上传的文件类型
- 上传的文件路径
- 上传图片缩略图大小


### 相关环境(推荐使用环境)
- OS Microsoft Windows 10 Pro
- Editor IntelliJ IDEA
- Java 8
- SpringMVC 4.3
- Mybitis 3.4
- Mysql 5.5.50
- Maven 3.5.3
- Git 2.14.1
- Tomcat 7.0.85
- Swagger 2.6.1
- Restful interface

