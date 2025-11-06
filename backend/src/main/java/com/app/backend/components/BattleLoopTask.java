package com.app.backend.components;

import com.app.backend.models.BattleState;
import com.app.backend.models.CauHoi;
import com.app.backend.models.TranDau;
import com.app.backend.models.constant.TrangThaiTranDau;
import com.app.backend.repositories.ITranDauRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Component
@RequiredArgsConstructor
public class BattleLoopTask {

    private final BattleStateManager battleStateManager;
    private final ITranDauRepository tranDauRepository;
    private final BattleWsPublisher wsPublisher;

    @Async
    public void runAutoLoop(Long tranDauId, int secondsPerQuestion) {
        BattleState state = battleStateManager.get(tranDauId);
        if (state == null) return;
        if (state.isAutoLoopRunning()) return;

        state.setAutoLoopRunning(true);
        battleStateManager.save(state);

        try {
            for (int i = 0; i < state.getDanhSachCauHoi().size(); i++) {
                state.setCurrentQuestionIndex(i);
                state.setCurrentQuestionStart(LocalDateTime.now());
                battleStateManager.save(state);

                // broadcast câu mới
                CauHoi q = state.getDanhSachCauHoi().get(i);
                wsPublisher.publishNewQuestion(tranDauId, i, q, secondsPerQuestion);

                Thread.sleep(secondsPerQuestion * 1000L);
            }

            // Kết thúc trận
            TranDau td = tranDauRepository.findById(tranDauId).orElse(null);
            if (td != null) {
                td.setTrangThai(TrangThaiTranDau.FINISHED);
                td.setKetThucLuc(LocalDateTime.now());
                tranDauRepository.save(td);
            }

            // Tạo leaderboard & winner nhanh tại đây? -> để service finishBattle làm chuẩn
            // Ở đây ta chỉ phát FINISHED sau khi service xử lý:
            // (gọi finishBattle để gom điểm & phát event finished đầy đủ)
            // Gợi ý: tiêm TranDauService vào và gọi finishBattle(..., autoMode=true).
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
