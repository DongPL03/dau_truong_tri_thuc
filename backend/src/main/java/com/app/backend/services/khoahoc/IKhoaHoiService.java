package com.app.backend.services.khoahoc;

import com.app.backend.dtos.KhoaHoiDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.PermissionDenyException;
import com.app.backend.models.KhoaHoc;
import com.app.backend.responses.khoahoc.KhoaHoiDetailResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

public interface IKhoaHoiService {

    KhoaHoc create(KhoaHoiDTO khoaHoiDTO, Long currentUserId) throws DataNotFoundException, PermissionDenyException;

    KhoaHoc update(Long id, KhoaHoiDTO khoaHoiDTO, Long currentUserId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException;

    void delete(Long id, Long currentUserId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException;

    KhoaHoc getById(Long id, Long currentUserId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException;

    KhoaHoiDetailResponse getDetailById(Long id, Long currentUserId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException;

    Page<KhoaHoc> findAll(PageRequest pageRequest,
                          String keyword,
                          Long chuDeId,
                          String trangThai,
                          Long creatorId,
                          boolean isAdmin);

    void softDelete(Long id, Long userId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException;
}

