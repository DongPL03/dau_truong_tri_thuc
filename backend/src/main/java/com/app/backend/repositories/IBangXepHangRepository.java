package com.app.backend.repositories;

import com.app.backend.models.BangXepHang;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.Instant;
import java.util.Optional;

public interface IBangXepHangRepository extends JpaRepository<BangXepHang, Long> {
    Optional<BangXepHang> findByNguoiDung_Id(Long nguoiDungId);

    Page<BangXepHang> findAllByOrderByTongDiemDescCapNhatLucAsc(Pageable pageable);

}