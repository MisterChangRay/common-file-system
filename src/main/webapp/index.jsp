<%@ page language="java" import="java.util.*" pageEncoding="utf-8"%>

<script src="https://cdn.bootcss.com/jquery/3.3.1/jquery.js"></script>
    <script>
        function download2() {
            var url = 'http://127.0.0.1:8080/v1/filesys/downloadFile?fileId=bdb035cc785a4499941695e4672e3d89&appKey=test1024&t=123&token=123&random=42';
            var xhr = new XMLHttpRequest();
            xhr.open('POST', url, true);        // 也可以使用POST方式，根据接口
            xhr.responseType = "blob";    // 返回类型blob
            xhr.setRequestHeader("Authorization", "111");
            // 定义请求完成的处理函数，请求前也可以增加加载框/禁用下载按钮逻辑
            xhr.onload = function () {
                // 请求完成
                if (this.status === 200) {
                    // 返回200
                    var blob = this.response;
                    var reader = new FileReader();
                    reader.readAsDataURL(blob);    // 转换为base64，可以直接放入a表情href
                    reader.onload = function (e) {
                        // 转换完成，创建一个a标签用于下载
                        var a = document.createElement('a');
                        a.download = 'test.jpg';
                        a.href = e.target.result;
                        $("body").append(a);    // 修复firefox中无法触发click
                        a.click();
                        $(a).remove();
                    }
                }
            };
            // 发送ajax请求
            xhr.send()
        }
    </script>



<html lang="en"><head>
    <meta charset="UTF-8">
    <title></title>
    <style id="system" type="text/css">h1,h2,h3,h4,h5,h6,p,blockquote {    margin: 0;    padding: 0;}body {    font-family: "Helvetica Neue", Helvetica, "Hiragino Sans GB", Arial, sans-serif;    font-size: 13px;    line-height: 18px;    color: #737373;    margin: 10px 13px 10px 13px;}a {    color: #0069d6;}a:hover {    color: #0050a3;    text-decoration: none;}a img {    border: none;}p {    margin-bottom: 9px;}h1,h2,h3,h4,h5,h6 {    color: #404040;    line-height: 36px;}h1 {    margin-bottom: 18px;    font-size: 30px;}h2 {    font-size: 24px;}h3 {    font-size: 18px;}h4 {    font-size: 16px;}h5 {    font-size: 14px;}h6 {    font-size: 13px;}hr {    margin: 0 0 19px;    border: 0;    border-bottom: 1px solid #ccc;}blockquote {    padding: 13px 13px 21px 15px;    margin-bottom: 18px;    font-family:georgia,serif;    font-style: italic;}blockquote:before {    content:"C";    font-size:40px;    margin-left:-10px;    font-family:georgia,serif;    color:#eee;}blockquote p {    font-size: 14px;    font-weight: 300;    line-height: 18px;    margin-bottom: 0;    font-style: italic;}code, pre {    font-family: Monaco, Andale Mono, Courier New, monospace;}code {    background-color: #fee9cc;    color: rgba(0, 0, 0, 0.75);    padding: 1px 3px;    font-size: 12px;    -webkit-border-radius: 3px;    -moz-border-radius: 3px;    border-radius: 3px;}pre {    display: block;    padding: 14px;    margin: 0 0 18px;    line-height: 16px;    font-size: 11px;    border: 1px solid #d9d9d9;    white-space: pre-wrap;    word-wrap: break-word;}pre code {    background-color: #fff;    color:#737373;    font-size: 11px;    padding: 0;}@media screen and (min-width: 768px) {    body {        width: 748px;        margin:10px auto;    }}</style><style id="custom" type="text/css"></style></head>
<body marginheight="0">
<button onclick="download2()">点击下载</button>
<button onclick="download2()">上传文件</button>
<button onclick="download2()">文件列表</button>
<h1>common-file-system</h1>
<p>一个独立的文件系统；提供小文件管理(30MB);便于分布式部署以提升性能;

</p>
<h5>已完成功能</h5>
<ul>
    <li>提供统一的文件管理方式;易于项目扩展和维护</li>
    <li>提供文件上传/打包下载/图片缩略图等功能</li>
    <li>可以使用独立的登录中心进行权限验证</li>
    <li>提供图形化管理界面;方便使用</li>
</ul>
<h5>快速开始</h5>
<ul>
    <li>clone项目到本地</li>
    <li>导入数据库并配置 <code>jdbc.properties</code> 文件中的数据库连接信息;</li>
    <li>引入maven配置中的依赖文件</li>
    <li>直接使用或重构项目</li>
</ul>
<h5>目前数据库信息,详细信息见"/resources/archives"目录下</h5>
<pre><code class="lang-sql">drop database common_file_system;

use common_file_system;

create database common_file_system;

create table common_authorize_code;

create table common_file;</code></pre>
<h5>可在<code>config.properties</code>中配置以下属性</h5>
<ul>
    <li>允许上传的文件大小</li>
    <li>允许上传的文件类型</li>
    <li>上传的文件路径</li>
    <li>上传图片缩略图大小</li>
</ul>
<h3>相关环境(推荐使用环境)</h3>
<ul>
    <li>OS Microsoft Windows 10 Pro</li>
    <li>Editor IntelliJ IDEA</li>
    <li>Java 8</li>
    <li>SpringMVC 4.3</li>
    <li>Mybitis 3.4</li>
    <li>Mysql 5.5.50</li>
    <li>Maven 3.5.3</li>
    <li>Git 2.14.1</li>
    <li>Tomcat 7.0.85</li>
    <li>Swagger 2.6.1</li>
    <li>Restful interface</li>
</ul>
<p>Edit By <a href="http://mahua.jser.me">MaHua</a></p>
</body></html>