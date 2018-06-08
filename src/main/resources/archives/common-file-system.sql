/*==============================================================*/
/* DBMS name:      MySQL 5.0                                    */
/* Created on:     2018/6/8 17:09:50                            */
/*==============================================================*/


drop table if exists common_authorize_code;

drop table if exists common_file;

/*==============================================================*/
/* Table: common_authorize_code                                 */
/*==============================================================*/
create table common_authorize_code
(
   id                   varchar(100),
   name                 varchar(100),
   description          varchar(100),
   code                 varchar(100),
   enabled              int comment '0false;1true',
   deleted              int comment '0false;1true'
);

alter table common_authorize_code comment '通过授权码结合独特算法判断该机构是否有上传下载权限';

/*==============================================================*/
/* Table: common_file                                           */
/*==============================================================*/
create table common_file
(
   id                   varchar(32) comment '文件ID;这里使用UUID',
   file_name            varchar(100) comment '上传文件的名称',
   file_size            varchar(100) comment '上传文件的大小',
   file_suffix          varchar(100) comment '上传文件的后缀',
   file_md5             varchar(100) comment '上传文件的MD5',
   file_type            varchar(100) comment '上传文件的类型',
   file_path            varchar(100) comment '上传文件保存位置',
   file_comp_path       varchar(100) comment '上传文件缩略图;图片才有',
   tag                  varchar(100) comment '上传文件标签',
   deleted              int comment '0false;1true'
);

