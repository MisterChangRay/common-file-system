package com.github.misterchangray.service.file.impl;

import com.github.misterchangray.common.PageInfo;
import com.github.misterchangray.common.ResultSet;
import com.github.misterchangray.dao.entity.File;
import com.github.misterchangray.dao.mapper.AuthorizeCodeMapper;
import com.github.misterchangray.dao.mapper.FileMapper;
import com.github.misterchangray.service.file.FileService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

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
    FileMapper fileMapper;
    @Autowired
    AuthorizeCodeMapper authorizeCodeMapper;


    public ResultSet exist(List<Integer> ids) {
        return null;
    }

    public ResultSet getById(Integer id) {
        return null;
    }

    public ResultSet getByIds(List<Integer> ids) {
        return null;
    }

    public ResultSet list(File file, PageInfo pageInfo) {
        return null;
    }

    public ResultSet insert(File file) {
        return null;
    }

    public ResultSet batchInsert(List<File> files) {
        return null;
    }

    public ResultSet update(File file) {
        return null;
    }

    public ResultSet delete(File file) {
        return null;
    }
}
