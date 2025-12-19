package com.app.backend.repositories;

import com.app.backend.models.BoCauHoiMoKhoa;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface IBoCauHoiMoKhoaRepository extends JpaRepository<BoCauHoiMoKhoa, Long> {

    boolean existsByNguoiDung_IdAndBoCauHoi_Id(Long nguoiDungId, Long boCauHoiId);

    Optional<BoCauHoiMoKhoa> findByNguoiDung_IdAndBoCauHoi_Id(Long userId, Long boCauHoiId);

    List<BoCauHoiMoKhoa> findAllByNguoiDung_Id(Long userId);
}
