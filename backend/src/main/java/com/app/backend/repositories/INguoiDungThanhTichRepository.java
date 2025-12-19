package com.app.backend.repositories;

import com.app.backend.models.NguoiDungThanhTich;
import com.app.backend.models.constant.AchievementCode;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface INguoiDungThanhTichRepository extends JpaRepository<NguoiDungThanhTich, Long> {

    boolean existsByNguoiDung_IdAndCode(Long nguoiDungId, AchievementCode code);

    List<NguoiDungThanhTich> findAllByNguoiDung_IdOrderByMoKhoaLucDesc(Long nguoiDungId);
}
