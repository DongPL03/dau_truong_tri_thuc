package com.app.backend.repositories;

import com.app.backend.models.PhienLuyenTap;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface IPhienLuyenTapRepository extends JpaRepository<PhienLuyenTap, Long> {
    Page<PhienLuyenTap> findByNguoiDungId(Long userId, Pageable pageable);

    @Query("SELECT COUNT(DISTINCT p.nguoiDung.id) FROM PhienLuyenTap p WHERE p.boCauHoi.id = :boCauHoiId")
    long countDistinctUsersByBoCauHoi(@Param("boCauHoiId") Long boCauHoiId);

    @Query("SELECT AVG(p.diemSo) FROM PhienLuyenTap p WHERE p.boCauHoi.id = :boCauHoiId")
    Double avgScoreByBoCauHoi(@Param("boCauHoiId") Long boCauHoiId);

}
