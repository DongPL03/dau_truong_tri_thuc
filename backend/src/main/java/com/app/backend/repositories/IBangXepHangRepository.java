package com.app.backend.repositories;

import com.app.backend.models.BangXepHang;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

public interface IBangXepHangRepository extends JpaRepository<BangXepHang, Long> {
    Optional<BangXepHang> findByNguoiDung_Id(Long nguoiDungId);

    Page<BangXepHang> findAllByOrderByTongDiemDescCapNhatLucAsc(Pageable pageable);


    // Native Query để update lại toàn bộ bảng xếp hạng
    @Modifying
    @Transactional
    @Query(value = """
            UPDATE bang_xep_hang b
            JOIN (
                SELECT id, 
                RANK() OVER (ORDER BY tong_diem DESC, so_tran_thang DESC, tong_tran ASC) as new_rank
                FROM bang_xep_hang
            ) r ON b.id = r.id
            SET b.xep_hang = r.new_rank
            """, nativeQuery = true)
    void updateAllRankings();

    long countByTongDiemGreaterThanAndNguoiDung_IdNot(Integer tongDiem, Long userId);
}