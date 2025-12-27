package com.app.backend.repositories;

import com.app.backend.models.CauHoi;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface ICauHoiRepository extends JpaRepository<CauHoi, Long> {
    @Query(value = "SELECT ch FROM CauHoi ch WHERE " +
            "(:boCauHoiId IS NULL OR :boCauHoiId = 0 OR  ch.boCauHoi.id = :boCauHoiId) " +
            "AND (:keyword IS NULL OR :keyword = '' OR LOWER(ch.noiDung) LIKE %:keyword%) " +
            "AND (:doKho IS NULL OR :doKho = '' OR ch.doKho = :doKho) " +
            "AND (:loaiNoiDung IS NULL OR :loaiNoiDung = '' OR ch.loaiNoiDung = :loaiNoiDung) " +
            "AND ( " +
            "       :isAdmin = true " +
            "       OR ch.boCauHoi.taoBoi.id = :creatorId" +
            "       OR ch.boCauHoi.trangThai = 'DA_DUYET'" +
            ")"
    )
    Page<CauHoi> searchCauHoi(
            @Param("boCauHoiId") Long boCauHoiId,
            @Param("keyword") String keyword,
            @Param("doKho") String doKho,
            @Param("loaiNoiDung") String loaiNoiDung,
            @Param("creatorId") Long creatorId,
            @Param("isAdmin") boolean isAdmin,
            Pageable pageable
    );

    Page<CauHoi> findByBoCauHoiId(Long boCauHoiId, Pageable pageable);

    List<CauHoi> findByBoCauHoiId(Long boCauHoiId);

    List<CauHoi> findByBoCauHoi_Id(Long id);
}

