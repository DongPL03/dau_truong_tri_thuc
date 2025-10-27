package com.app.backend.services.cauhoi;

import com.app.backend.dtos.CauHoiDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.PermissionDenyException;
import com.app.backend.models.CauHoi;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.repository.query.Param;

public interface ICauHoiService {
    CauHoi create(CauHoiDTO dto, Long currentUserId, boolean isAdmin);

    CauHoi update(Long id, CauHoiDTO dto, Long currentUserId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException;

    void delete(Long id, Long currentUserId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException;

    Page<CauHoi> findAll(Long boCauHoiId,
                         String keyword,
                         String doKho,
                         String loaiNoiDung,
                         Long creatorId,
                         boolean isAdmin,
                         PageRequest pageRequest);

    void changeMedia(Long cauHoiId, String duongDanTep) throws DataNotFoundException;
}
