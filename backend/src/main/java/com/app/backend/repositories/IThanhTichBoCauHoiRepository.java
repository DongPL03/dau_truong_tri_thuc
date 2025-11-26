package com.app.backend.repositories;

import com.app.backend.models.ThanhTichBoCauHoi;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface IThanhTichBoCauHoiRepository extends JpaRepository<ThanhTichBoCauHoi, Long> {

    Optional<ThanhTichBoCauHoi> findByNguoiDung_IdAndBoCauHoi_Id(
            Long nguoiDungId,
            Long boCauHoiId
    );
}
