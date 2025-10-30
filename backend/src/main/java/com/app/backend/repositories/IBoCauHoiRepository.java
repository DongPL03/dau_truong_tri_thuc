package com.app.backend.repositories;

import com.app.backend.models.BoCauHoi;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface IBoCauHoiRepository extends JpaRepository<BoCauHoi, Long> {
    @Query("SELECT b FROM BoCauHoi b WHERE " +
            "b.isDelete = false AND " +
            "(:keyword IS NULL OR :keyword = '' OR LOWER(b.tieuDe) LIKE %:keyword%) " +
            "AND (:chuDeId IS NULL OR :chuDeId = 0 OR b.chuDe.id = :chuDeId) " +
            "AND (:cheDoHienThi IS NULL OR :cheDoHienThi = '' OR b.cheDoHienThi = :cheDoHienThi) " +
            "AND ( " +
            "   :isAdmin = true " + // Nếu là Admin, điều kiện này luôn đúng, và lấy hết
//            "   OR b.trangThai = com.app.backend.models.enums.TrangThaiBoCauHoi.DA_DUYET " + // HOẶC, lấy các bài đã được duyệt (dành cho User thường)
            "     OR b.taoBoi.id = :creatorId " + // HOẶC, lấy các bài của chính người dùng hiện tại (kể cả chưa duyệt)
            ") "
    )
    Page<BoCauHoi> findAll(Pageable pageable,
                           @Param("keyword") String keyword,
                           @Param("chuDeId") Long chuDeId,
                           @Param("cheDoHienThi") String cheDoHienThi,
                           @Param("creatorId") Long creatorId,
                           @Param("isAdmin") boolean isAdmin);
}
