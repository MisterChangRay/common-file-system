package com.github.misterchangray.service.file.impl;

import com.github.misterchangray.common.PageInfo;
import com.github.misterchangray.common.ResultSet;
import com.github.misterchangray.common.enums.DBEnum;
import com.github.misterchangray.common.enums.ResultEnum;
import com.github.misterchangray.common.utils.CryptoUtils;
import com.github.misterchangray.common.utils.DateUtils;
import com.github.misterchangray.dao.entity.CommonAuthorizeCode;
import com.github.misterchangray.dao.entity.CommonAuthorizeCodeQuery;
import com.github.misterchangray.dao.entity.CommonFile;
import com.github.misterchangray.dao.entity.CommonFileQuery;
import com.github.misterchangray.dao.mapper.CommonAuthorizeCodeMapper;
import com.github.misterchangray.dao.mapper.CommonFileMapper;
import com.github.misterchangray.service.file.FileService;
import com.github.misterchangray.service.file.dto.FileInfo;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * 文件服务实现类
 *
 * @author Rui.Zhang/misterchangray@hotmail.com
 * @author Created on 3/26/2018.
 */
@Service
public class FileServiceImpl implements FileService {
    @Autowired
    CommonFileMapper commonFileMapper;
    @Autowired
    CommonAuthorizeCodeMapper commonAuthorizeCodeMapper;
    @Value("${file.base.path:d:/upload}")
    private String baseUploadPath;



    public ResultSet saveAll(List<CommonFile> commonFiles) { return null; }

    public ResultSet edit(CommonFile commonFile) { return null; }

    public ResultSet exist(List<Integer> ids) {
        return null;
    }

    public ResultSet getById(Integer id) {
        return null;
    }

    public ResultSet getByIds(List<Integer> ids) {
        return null;
    }

    public ResultSet list(CommonFile commonFile, PageInfo pageInfo) {
        if(null == pageInfo) pageInfo = new PageInfo();

        CommonFileQuery commonFileQuery = new CommonFileQuery();
        commonFileQuery.page(pageInfo.getPage(), pageInfo.getLimit());

        CommonFileQuery.Criteria criteria = commonFileQuery.createCriteria();
        criteria.andDeletedEqualTo(DBEnum.FALSE.getCode());

        pageInfo.setCount(commonFileMapper.countByQuery(commonFileQuery));
        return ResultSet.build().setData(commonFileMapper.selectByQuery(commonFileQuery)).setPageInfo(pageInfo);
    }

    public ResultSet save(CommonFile commonFile) {
        commonFileMapper.insert(commonFile);
        return ResultSet.build(ResultEnum.SUCCESS);
    }


    public ResultSet delete(CommonFile commonFile) {
        if(null != commonFile && null != commonFile.getId()) {
            CommonFileQuery commonFileQuery = new CommonFileQuery();
            CommonFileQuery.Criteria fileCriteria = commonFileQuery.createCriteria();
            fileCriteria.andFileMd5EqualTo(commonFile.getFileMd5());

            if(1 == commonFileMapper.countByQuery(commonFileQuery)) {
                CommonFile updateData = new CommonFile();
                updateData.setId(commonFile.getId());
                updateData.setDeleted(DBEnum.TRUE.getCode());

                commonFileMapper.updateByQuerySelective(commonFile, commonFileQuery);
            }
            return ResultSet.build(ResultEnum.SUCCESS);
        }
        return ResultSet.build(ResultEnum.FAILURE);

    }

    /**
     * 保存文件到硬盘
     * @param uploadFile
     * @param appKey
     * @return
     * @throws IOException
     */
    public ResultSet<CommonFile> saveFile(MultipartFile uploadFile, String appKey) throws IOException {
        if(!uploadFile.isEmpty() && null != appKey) {
            //验证appKey
            if(false == this.existAppKey(appKey)) return ResultSet.build(ResultEnum.INVALID_REQUEST);

            //验证是否有相同文件;有则直接返回
            String fileSign = CryptoUtils.encodeMD5(uploadFile.getBytes());
            CommonFile commonFile;
            if(null != (commonFile = this.existFile(fileSign))) return ResultSet.build(ResultEnum.SUCCESS).setData(commonFile);

            //保存上传文件信息
            commonFile = new CommonFile();
            commonFile.setId(CryptoUtils.getUUID());
            commonFile.setFileSuffix(getSuffix(uploadFile.getOriginalFilename()));
            commonFile.setFilePath(buildFilePath() + commonFile.getId() + commonFile.getFileSuffix());
            commonFile.setFileName(uploadFile.getOriginalFilename());
            commonFile.setFileSize(String.valueOf(uploadFile.getSize()));
            commonFile.setFileType(uploadFile.getContentType());
            commonFile.setFileMd5(fileSign);
            commonFile.setDeleted(DBEnum.FALSE.getCode());
            if(0 != commonFileMapper.insert(commonFile)) {
                //生成文件路径; baseUploadPath + uuid + 文件后缀
                String path = baseUploadPath + commonFile.getFilePath();
                //保存上传文件
                uploadFile.transferTo(new File(path));

                return ResultSet.build(ResultEnum.SUCCESS).setData(commonFile);
            } else {
                return ResultSet.build(ResultEnum.SERVER_ERROR);
            }
        } else {
            return ResultSet.build(ResultEnum.INVALID_REQUEST);
        }
    }



    /**
     * 打包文件成zip包
     * @param fileInfos
     * @param zipName
     * @param appKey
     * @return
     * @throws Exception
     */
    public ResultSet<CommonFile> packFilesToZip(List<FileInfo> fileInfos, String zipName, String appKey) throws Exception {
        if(null == fileInfos) ResultSet.build(ResultEnum.INVALID_REQUEST);
        if(null == appKey) ResultSet.build(ResultEnum.INVALID_REQUEST);

        //验证appKey
        if(false == this.existAppKey(appKey)) return ResultSet.build(ResultEnum.INVALID_REQUEST);

        String zipFilePath = this.packZipFile(fileInfos, zipName);
        File file = new File(zipFilePath);
        if(null != zipFilePath) {
            //保存压缩文件信息
            CommonFile commonFile = new CommonFile();
            commonFile.setId(CryptoUtils.getUUID());
            commonFile.setFileName(file.getName());
            commonFile.setFileSize(String.valueOf(file.length()));
            commonFile.setFileSuffix(getSuffix(file.getName()));
            commonFile.setFilePath(zipFilePath.replaceAll(baseUploadPath, ""));
            commonFile.setFileType("application/zip");
            commonFile.setDeleted(DBEnum.FALSE.getCode());
//            this.save(commonFile); //todo 暂不保存压缩文件

            return ResultSet.build(ResultEnum.SUCCESS).setData(commonFile);
        }
        return ResultSet.build(ResultEnum.INVALID_REQUEST);
    }

    /**
     * 验证文件是否存在
     * @param fileMd5
     * @return
     * 如存在则直接返回文件信息;如不存在则返回null
     */
    private CommonFile existFile(String fileMd5) {
        //验证是否有相同文件;有则直接返回
        String fileSign = fileMd5;
        CommonFileQuery commonFileQuery = new CommonFileQuery();
        CommonFileQuery.Criteria fileCriteria = commonFileQuery.createCriteria();
        fileCriteria.andFileMd5EqualTo(fileSign);
        List<CommonFile> commonFiles = commonFileMapper.selectByQuery(commonFileQuery);
        if(0 != commonFiles.size()) {
            return commonFiles.get(0);
        }
        return null;
    }

    /**
     * 验证Appkey是否有效
     * @param appKey
     * @return true有效;false无效
     */
    private boolean existAppKey(String appKey) {
        //验证appKey
        CommonAuthorizeCodeQuery commonAuthorizeCodeQuery = new CommonAuthorizeCodeQuery();
        CommonAuthorizeCodeQuery.Criteria criteria = commonAuthorizeCodeQuery.createCriteria();
        criteria.andCodeEqualTo(appKey);
        criteria.andDeletedEqualTo(DBEnum.FALSE.getCode());
        criteria.andEnabledEqualTo(DBEnum.TRUE.getCode());

        List<CommonAuthorizeCode> commonAuthorizeCodes = commonAuthorizeCodeMapper.selectByQuery(commonAuthorizeCodeQuery);
        if(0 == commonAuthorizeCodes.size()) return false;
        return true;
    }


    /**
     * 打包成Zip文件
     * @param fileInfos 被压缩文件信息
     * @param zipName   压缩文件名
     * @return 返回压缩文件名
     * @throws RuntimeException 压缩失败会抛出运行时异常
     */
    private String packZipFile(List<FileInfo>  fileInfos, String zipName) throws Exception{
        if(null == fileInfos || 0 == fileInfos.size()) return null;
        byte[] buf = new byte[2 * 1024];
        File sourceFile;
        String path, filename, target, zipFilePath = baseUploadPath + "/zipFiles/";
        if(null != zipName && !"".equals(zipName)) {
            zipFilePath += CryptoUtils.getUUID() + "/" + zipName;
        } else {
            zipFilePath += CryptoUtils.getUUID();
        }
        if(!zipName.contains(".")) zipFilePath += ".zip";

        List<CommonFile> commonFiles;
        CommonFile commonFile;
        File file = new File(zipFilePath);
        if(!file.getParentFile().exists()) file.getParentFile().mkdirs();

        FileOutputStream out = new FileOutputStream(file);
        ZipOutputStream zos = new ZipOutputStream(out);

        for(FileInfo fileInfo : fileInfos) {
            CommonFileQuery commonFileQuery = new CommonFileQuery();
            CommonFileQuery.Criteria commonFileQueryCriteria = commonFileQuery.createCriteria();
            commonFileQueryCriteria.andIdEqualTo(fileInfo.getFileId());
            commonFiles = commonFileMapper.selectByQuery(commonFileQuery);
            if(null != commonFiles && 1 == commonFiles.size()) {
                commonFile = commonFiles.get(0);
                sourceFile = new File(getFilePath(commonFile));

                if(null != (target = fileInfo.getTarget())) {
                    filename = target.replaceAll(".*[\\/]","");
                    path = target.substring(0 ,target.length() - filename.length());

                    if(null != path && !"".equals(path)) {
                        path = path.replaceFirst("[\\/]", "");
                    }
                    if(null == filename || "".equals(filename)) {
                        zos.putNextEntry(new ZipEntry(path + commonFile.getFileName()));
                    } else {
                        zos.putNextEntry(new ZipEntry(path + filename));
                    }
                } else {
                    zos.putNextEntry(new ZipEntry(commonFile.getFileName()));
                }

                // copy文件到zip输出流中
                int len;
                FileInputStream in = new FileInputStream(sourceFile);
                while ((len = in.read(buf)) != -1){
                    zos.write(buf, 0, len);
                }
                // Complete the entry
                in.close();
            }
        }
        zos.finish();
        zos.closeEntry();
        out.flush();
        out.close();

        return zipFilePath;
    }

    /**
     * 获取文件后缀
     * @return
     */
    private String getSuffix(String filename) {
        if (null == filename) return "";
        return filename.replaceAll("^.+(?=\\.)", "");
    }

    /**
     * 创建文件保存的路径
     * @return
     */
    private String buildFilePath() {
        StringBuilder path = new StringBuilder();
        path.append("/");
        path.append(DateUtils.now("yyyy-MM-dd"));
        path.append("/");

        File filePath = new File(path.toString());
        //判断路径是否存在，如果不存在就创建一个
        if (!filePath.exists()) {
            filePath.mkdirs();
        }
        return path.toString();
    }

    /**
     * 根据文件信息获取文件保存的路径
     * @return
     */
    private String getFilePath(CommonFile commonFile) {
        String path = baseUploadPath + commonFile.getFilePath();
        File filePath = new File(path);
        //判断路径是否存在，如果不存在就创建一个
        if (!filePath.exists()) {
            filePath.mkdirs();
        }
        return path;
    }


}
