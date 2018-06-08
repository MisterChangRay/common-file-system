package com.github.misterchangray.service.file.impl;

import com.github.misterchangray.common.PageInfo;
import com.github.misterchangray.common.ResultSet;
import com.github.misterchangray.common.enums.DBEnum;
import com.github.misterchangray.common.enums.ResultEnum;
import com.github.misterchangray.common.utils.CryptoUtils;
import com.github.misterchangray.common.utils.DateUtils;
import com.github.misterchangray.dao.entity.CommonFile;
import com.github.misterchangray.dao.entity.CommonFileQuery;
import com.github.misterchangray.dao.mapper.CommonAuthorizeCodeMapper;
import com.github.misterchangray.dao.mapper.CommonFileMapper;
import com.github.misterchangray.service.file.FileService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;
import java.util.List;

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
    private String basePath;


    public ResultSet list(CommonFile commonFile, PageInfo pageInfo) {
        if(null == pageInfo) pageInfo = new PageInfo();

        CommonFileQuery commonFileQuery = new CommonFileQuery();
        commonFileQuery.page(pageInfo.getPage(), pageInfo.getLimit());

        CommonFileQuery.Criteria criteria = commonFileQuery.createCriteria();
        criteria.andDeletedEqualTo(DBEnum.FALSE.getCode());

        pageInfo.setCount(commonFileMapper.countByQuery(commonFileQuery));
        return ResultSet.build().setData(commonFileMapper.selectByQuery(commonFileQuery)).setPageInfo(pageInfo);
    }

    public ResultSet insert(CommonFile commonFile) {
        commonFileMapper.insert(commonFile);
        return ResultSet.build(ResultEnum.SUCCESS);
    }


    public ResultSet delete(CommonFile commonFile) {
        String path = getFilePath(commonFile);
        File filePath = new File(path);
        filePath.delete();
        return ResultSet.build(ResultEnum.SUCCESS);
    }

    public ResultSet<CommonFile> saveFile(MultipartFile uploadFile) throws IOException {
        if(!uploadFile.isEmpty()) {
            CommonFile commonFile = new CommonFile();
            commonFile.setId(CryptoUtils.getUUID());
            commonFile.setFilePath(buildFilePath());
            commonFile.setFileName(uploadFile.getOriginalFilename());
            commonFile.setFileSize(String.valueOf(uploadFile.getSize()));
            commonFile.setFileSuffix(getSuffix(uploadFile.getOriginalFilename()));
            commonFile.setFileType(uploadFile.getContentType());
            commonFile.setFileMd5(CryptoUtils.encodeMD5(uploadFile.getBytes()));
            if(0 != commonFileMapper.insert(commonFile)) {
                //生成文件路径; basePath + uuid + 文件后缀
                String path = commonFile.getFilePath() + commonFile.getId() + commonFile.getFileSuffix();
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
        path.append(basePath);
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
        String path = basePath + commonFile.getFilePath();
        File filePath = new File(path);
        //判断路径是否存在，如果不存在就创建一个
        if (filePath.exists()) {
            filePath.mkdirs();
        }
        return path;
    }



    public ResultSet batchInsert(List<CommonFile> commonFiles) { return null; }

    public ResultSet update(CommonFile commonFile) { return null; }

    public ResultSet exist(List<Integer> ids) {
        return null;
    }

    public ResultSet getById(Integer id) {
        return null;
    }

    public ResultSet getByIds(List<Integer> ids) {
        return null;
    }
}