package com.app.backend.services.bocauhoi;

import com.app.backend.dtos.BoCauHoiDTO;
import com.app.backend.dtos.TuChoiBoCauHoiDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.PermissionDenyException;
import com.app.backend.models.BoCauHoi;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

public interface IBoCauHoiService {
    BoCauHoi create(BoCauHoiDTO boCauHoiDTO, Long currentUserId) throws DataNotFoundException;

    BoCauHoi update(Long id, BoCauHoiDTO boCauHoiDTO, Long currentUserId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException;

    void delete(Long id, Long currentUserId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException;

    BoCauHoi getById(Long id, Long currentUserId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException;

    //    @Query("SELECT b FROM BoCauHoi b WHERE (:keyword IS NULL OR :keyword = '' OR" +
//            "b.tieuDe LIKE %:keyword% OR" +
//            "b.noiDung LIKE %:keyword%)" +
//            "AND (:chuDeId IS NULL OR b.chuDe.id = :chuDeId)" +
//            "AND (:cheDoHienThi IS NULL OR :cheDoHienThi = '' OR" +
//            "b.cheDoHienThi = :cheDoHienThi)" +
//            "AND (:onlyMine = FALSE OR b.nguoiTao.id = :currentUserId)")
//    @Query("SELECT b FROM BoCauHoi b WHERE " +
//            "(:keyword IS NULL OR :keyword = '' OR " +
//            "LOWER(b.tieuDe) LIKE %:keyword% OR " +
//            "LOWER(b.moTa) LIKE %:keyword%)" +
//            "AND (:chuDeId IS NULL OR :chuDeId = 0 OR b.chuDe.id = :chuDeId)" +
//            "AND (b.trangThai = 'DA_DUYET')")
    Page<BoCauHoi> findAll(PageRequest pageRequest, String keyword,
                           Long chuDeId,
                           String cheDoHienThi,
                           Long creatorId,
                           boolean isAdmin);

    BoCauHoi approve(Long id, Long adminId);

    BoCauHoi reject(Long id, TuChoiBoCauHoiDTO lyDoTuChoi, Long adminId);

    void softDelete(Long id, Long userId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException;

//    BoCauHoi markOfficial(Long id, Long adminId);
}
