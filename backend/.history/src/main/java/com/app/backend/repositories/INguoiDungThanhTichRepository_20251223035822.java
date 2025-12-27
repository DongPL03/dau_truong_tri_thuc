package com.app.backend.repositories;

import aj.org.objectweb.asm.commons.Remapper;
import com.app.backend.models.NguoiDungThanhTich;
import com.app.backend.models.constant.AchievementCode;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface INguoiDungThanhTichRepository extends JpaRepository<NguoiDungThanhTich, Long> {

    boolean existsByNguoiDung_IdAndCode(Long nguoiDungId, AchievementCode code);

    Optional<NguoiDungThanhTich> findByNguoiDung_IdAndCode(Long nguoiDungId, AchievementCode code);

    List<NguoiDungThanhTich> findAllByNguoiDung_IdOrderByMoKhoaLucDesc(Long nguoiDungId);
    
}
